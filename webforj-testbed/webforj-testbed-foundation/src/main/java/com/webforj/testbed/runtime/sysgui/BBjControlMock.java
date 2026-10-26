package com.webforj.testbed.runtime.sysgui;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGainedFocusEvent;
import com.basis.bbj.proxies.event.BBjLostFocusEvent;
import com.basis.bbj.proxies.sysgui.TextAlignable;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.google.gson.Gson;
import com.webforj.testbed.runtime.RuntimeMock;
import com.webforj.testbed.runtime.event.BBjControlEventCallbackRegistry;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * The mock backing the base control surface every component relies on.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjControlMock extends RuntimeMock {
  private final int id;
  private final String clientObjectId = UUID.randomUUID().toString();
  private static final String FOCUS_PROPERTY = "hasFocus";
  private static final String CLASS_ATTRIBUTE = "class";
  private static final String CLASS_NAME_SEPARATOR = " ";
  // The pattern the runtime validates class names with.
  private static final Pattern CLASS_NAME_PATTERN =
      Pattern.compile("-?[0-\\uD7FF\\uE000-\\uFFFD][-0-\\uD7FF\\uE000-\\uFFFD]*");
  private static final Gson GSON = new Gson();
  private final Map<String, String> properties = new ConcurrentHashMap<>();
  private final Map<String, String> attributes = new ConcurrentHashMap<>();
  private final Map<String, String> cssStyles = new ConcurrentHashMap<>();
  private final Map<Object, Object> clientProperties = new ConcurrentHashMap<>();
  private final List<String> styleNames = new ArrayList<>();
  private final Set<String> classNames = new LinkedHashSet<>();
  private final BBjControlEventCallbackRegistry callbacks = new BBjControlEventCallbackRegistry();
  private BBjWindowMock parent;
  private boolean destroyed = false;
  private boolean focused;

  /**
   * Creates a new control mock.
   *
   * @param id the control identifier
   */
  BBjControlMock(int id) {
    this.id = id;
    setValue("visible", true);
    setValue("enabled", true);
    setValue("editable", true);
    // Text is left aligned until the component says otherwise, as the runtime reports it.
    setValue("alignment", TextAlignable.ALIGN_LEFT);
  }

  /**
   * Returns the control identifier.
   *
   * @return the control identifier
   */
  public int getId() {
    return id;
  }

  /**
   * Returns whether the control has been destroyed.
   *
   * @return true when the control has been destroyed
   */
  public boolean isDestroyed() {
    return destroyed;
  }

  /**
   * Returns the properties written through the control.
   *
   * @return the property map
   */
  Map<String, String> getProperties() {
    return properties;
  }

  /**
   * Returns the attributes written through the control.
   *
   * @return the attribute map
   */
  public Map<String, String> getAttributes() {
    return attributes;
  }

  /**
   * Sets the parent window mock of this control.
   *
   * @param parent the parent window mock
   */
  void setParent(BBjWindowMock parent) {
    this.parent = parent;
  }

  /**
   * Returns the parent window mock of this control.
   *
   * @return the parent window mock, or an empty optional for a top level window
   */
  public Optional<BBjWindowMock> getParent() {
    return Optional.ofNullable(parent);
  }

  /**
   * Stores the text the control shows.
   *
   * @param text the text
   */
  void putText(String text) {
    putState("text", text);
  }

  /**
   * Writes a value into the state of this control.
   *
   * <p>
   * The value is read back by the accessor whose derived key matches, so writing the key
   * {@code selected} makes {@code isSelected()} return the value.
   * </p>
   *
   * @param key the state key
   * @param value the value to store
   */
  void putState(String key, Object value) {
    setValue(key, value);
  }

  /**
   * Reads a value from the state of this control.
   *
   * @param key the state key
   *
   * @return the stored value, or null when absent
   */
  Object getState(String key) {
    return getValue(key);
  }

  /**
   * Fires a control event through the callback registered for the given event type.
   *
   * <p>
   * The event reaches the component exactly as a client originated event would, through the
   * callback the component registered on the control.
   * </p>
   *
   * @param eventType the event type constant the callback was registered under
   * @param event the event to deliver
   *
   * @return true when a callback was registered and invoked, false otherwise
   */
  public boolean fireEvent(int eventType, BBjEvent event) {
    return callbacks.fire(eventType, event);
  }

  /**
   * Returns whether a callback is registered for the given control event type.
   *
   * @param eventType the event type constant
   *
   * @return true when a callback is registered
   */
  boolean hasCallback(int eventType) {
    return callbacks.has(eventType);
  }

  public BBjControlEventCallbackRegistry getCallbacks() {
    return callbacks;
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "getID" -> id;
      case "getClientObjectID" -> clientObjectId;
      case "destroy" -> {
        destroyed = true;
        // The window forgets a destroyed control and frees its id.
        if (parent != null) {
          parent.removeChild(this);
        }
        yield null;
      }
      case "focus" -> {
        // The runtime ignores the request while the control or a window above it is disabled.
        if (isEnabledUpwards()) {
          takeFocus();
        }
        yield null;
      }
      case "isDestroyed" -> destroyed;
      case "setProperty" -> {
        // The runtime carries a property as JSON, so a value comes back as the type the reader
        // asks for, the way the browser side would hand it over.
        putOrRemove(properties, (String) args[0], args[1] == null ? null : GSON.toJson(args[1]));
        yield null;
      }
      case "getProperty" -> readProperty(args);
      case "hasAttribute" -> attributes.containsKey(args[0]);
      case "setAttribute" -> {
        putOrRemove(attributes, (String) args[0], (String) args[1]);
        if (CLASS_ATTRIBUTE.equals(args[0])) {
          readClassNames((String) args[1]);
        }
        yield null;
      }
      case "getAttribute" -> attributes.get(args[0]);
      case "removeAttribute" -> {
        attributes.remove(args[0]);
        if (CLASS_ATTRIBUTE.equals(args[0])) {
          classNames.clear();
        }
        yield null;
      }
      case "putClientProperty" -> {
        putOrRemove(clientProperties, args[0], args[1]);
        yield null;
      }
      case "getClientProperty" -> clientProperties.get(args[0]);
      case "setStyle" -> {
        putOrRemove(cssStyles, (String) args[0], (String) args[1]);
        yield null;
      }
      case "getStyle", "getComputedStyle" -> cssStyles.get(args[0]);
      case "unsetStyle" -> {
        cssStyles.remove(args[0]);
        yield null;
      }
      case "addStyle" -> styleNames.add((String) args[0]);
      case "removeStyle" -> styleNames.remove(args[0]);
      case "clearStyles" -> {
        styleNames.clear();
        yield null;
      }
      case "getStyles" -> new BBjVector(styleNames);
      case "addClass" -> {
        classNames.addAll(classNamesOf((String) args[0]));
        writeClassNames();
        yield null;
      }
      case "removeClass" -> {
        classNames.removeAll(classNamesOf((String) args[0]));
        writeClassNames();
        yield null;
      }
      case "toggleClass" -> {
        String className = (String) args[0];
        checkClassName(className);
        boolean added = classNames.add(className);
        if (!added) {
          classNames.remove(className);
        }
        writeClassNames();
        yield added;
      }
      case "getClassList" -> new BBjVector(classNames);
      case "getParentWindow" -> parent == null ? null : parent.getProxy(BBjWindowMock.WINDOW_TYPE);
      case "setCallback" -> {
        if (args.length == 3 && args[0] instanceof Integer eventType
            && args[2] instanceof String methodName) {
          callbacks.register(eventType, args[1], methodName);
          yield null;
        }
        yield UNHANDLED;
      }
      case "clearCallback" -> {
        if (args.length == 1 && args[0] instanceof Integer eventType) {
          callbacks.clear(eventType);
          yield null;
        }
        yield UNHANDLED;
      }
      default -> UNHANDLED;
    };
  }

  private Object readProperty(Object[] args) {
    // the focus is held by the runtime, it is not a property the server wrote
    if (FOCUS_PROPERTY.equals(args[0])) {
      return focused;
    }

    String json = properties.get(args[0]);
    Type type = args.length == 2 && args[1] instanceof Type requested ? requested : null;
    if (json == null) {
      return null;
    }

    return GSON.fromJson(json, type == null ? Object.class : type);
  }

  private static <K, V> void putOrRemove(Map<K, V> store, K key, V value) {
    if (value == null) {
      store.remove(key);
    } else {
      store.put(key, value);
    }
  }

  /**
   * Gives this control the focus without the client reporting it, for a focus the application
   * already knows about.
   */
  public void holdFocus() {
    root().clearFocus();
    focused = true;
  }

  // The browser holds one focused element: the control that had it reports losing it, then this
  // control takes it and reports gaining it.
  private void takeFocus() {
    BBjControlMock holder = root().findFocused();
    if (holder == this) {
      return;
    }

    if (holder != null) {
      holder.focused = false;
      holder.reportFocus(SysGuiEventConstants.ON_LOST_FOCUS, BBjLostFocusEvent.class);
    }

    holdFocus();
    reportFocus(SysGuiEventConstants.ON_GAINED_FOCUS, BBjGainedFocusEvent.class);
  }

  private void reportFocus(int eventType, Class<? extends BBjEvent> type) {
    if (!callbacks.has(eventType)) {
      return;
    }

    Map<String, Object> returnValues = new LinkedHashMap<>();
    returnValues.put("getText", getValue("text", ""));
    returnValues.put("isTemporary", false);
    callbacks.fire(eventType, RuntimeMock.createEvent(type, returnValues));
  }

  private BBjControlMock root() {
    BBjControlMock root = this;
    while (root.parent != null) {
      root = root.parent;
    }

    return root;
  }

  private BBjControlMock findFocused() {
    if (focused) {
      return this;
    }

    if (this instanceof BBjWindowMock window) {
      for (BBjControlMock child : window.getChildren()) {
        BBjControlMock focusedChild = child.findFocused();
        if (focusedChild != null) {
          return focusedChild;
        }
      }
    }

    return null;
  }

  private boolean isEnabledUpwards() {
    BBjControlMock current = this;
    while (current != null) {
      if (Boolean.FALSE.equals(current.getValue("enabled"))) {
        return false;
      }
      current = current.getParent().orElse(null);
    }

    return true;
  }

  private void clearFocus() {
    focused = false;
    if (this instanceof BBjWindowMock window) {
      for (BBjControlMock child : window.getChildren()) {
        child.clearFocus();
      }
    }
  }

  // The runtime validates every name before it applies the first one, so one invalid name leaves
  // the class list as it was.
  private static List<String> classNamesOf(String names) throws BBjException {
    String[] split = names.split(CLASS_NAME_SEPARATOR);
    for (String name : split) {
      checkClassName(name);
    }

    return List.of(split);
  }

  private static void checkClassName(String name) throws BBjException {
    if (!CLASS_NAME_PATTERN.matcher(name).matches()) {
      throw new BBjException("Invalid class name: " + name, BBjException.ARG_ERR);
    }
  }

  private void readClassNames(String value) {
    classNames.clear();
    if (value != null && !value.isBlank()) {
      classNames.addAll(List.of(value.trim().split("\\s+")));
    }
  }

  // The class list and the class attribute are one state in the browser, which leaves an element
  // without the attribute as it is while the list stays empty.
  private void writeClassNames() {
    if (classNames.isEmpty() && !attributes.containsKey(CLASS_ATTRIBUTE)) {
      return;
    }

    attributes.put(CLASS_ATTRIBUTE, String.join(CLASS_NAME_SEPARATOR, classNames));
  }
}
