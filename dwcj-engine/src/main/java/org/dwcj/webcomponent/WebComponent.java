package org.dwcj.webcomponent;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.AbstractMap.SimpleEntry;
import java.util.Map.Entry;
import java.util.function.UnaryOperator;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.annotation.InlineStyleSheet;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.htmlcontainer.event.HtmlContainerJavascriptEvent;
import org.dwcj.component.panels.AbstractPanel;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.ComponentDestroyed;
import org.dwcj.exceptions.DwcjRuntimeException;

import org.dwcj.webcomponent.annotations.NodeAttribute;
import org.dwcj.webcomponent.annotations.NodeClassName;
import org.dwcj.webcomponent.annotations.EventExpressions;
import org.dwcj.webcomponent.annotations.EventName;
import org.dwcj.webcomponent.annotations.HtmlViewAttribute;
import org.dwcj.webcomponent.annotations.HtmlViewClassName;
import org.dwcj.webcomponent.annotations.NodeName;
import org.dwcj.webcomponent.annotations.NodeProperty;
import org.dwcj.webcomponent.events.Event;
import org.dwcj.webcomponent.events.EventDispatcher;
import org.dwcj.webcomponent.events.EventListener;

/**
 * The class WebComponent helps to integrate web components into the DWCJ
 * framework.
 * 
 * The web component class must be annotated with {@link NodeName} annotation to
 * specify the tag name of the web component and must extend this class.
 * 
 * Attributes and properties that must be set directly after creation can
 * use the {@link NodeAttribute} and {@link NodeProperty} annotations.
 * 
 * To include assets (JavaScript, CSS, etc.) for the web component, the class
 * can be annotated with assets annotations like the {@link JavaScript} and
 * {@link InlineStyleSheet} annotations or by using the {@link org.dwcj.App}
 * class API.
 * 
 * @author Hyyan Abo Fakher
 */
public abstract class WebComponent extends AbstractComponent {
  private final HtmlContainer hv;
  private final String uuid = UUID.randomUUID().toString().substring(0, 8);
  private final Map<String, Object> properties = new HashMap<>();
  private final Map<String, String> attributes = new HashMap<>();
  private final ArrayList<String> asyncScripts = new ArrayList<>();
  private final ArrayList<String> registeredClientEvents = new ArrayList<>();
  private final HashMap<String, Class<? extends Event<?>>> clientEventMap = new HashMap<>();
  private final Map<String, String> rawSlots = new HashMap<>();
  private final Map<String, Entry<AbstractPanel, Boolean>> slots = new HashMap<>();
  private final Map<AbstractComponent, Entry<String, Boolean>> controls = new HashMap<>();
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final UnaryOperator<String> encode = (value) -> {
    return Base64.getEncoder().encodeToString(String.valueOf(value).getBytes());
  };
  private AbstractPanel panel;

  /**
   * Create a new instance of the web component.
   */
  protected WebComponent() {
    super();

    hv = new HtmlContainer();
    hv.setTabTraversable(false);
    hv.setFocusable(false);
    hv.onJavascriptEvent(this::handleJavascriptEvents);
    hv.setAttribute("dwcj-hv", getUUID());

    // bbj-remove is a special attribute in DWC.
    // If the child element isn't contained in the specified parent, go ahead and
    // remove it anyway if either the child or actual parent contains the attribute
    // "bbj-remove".
    hv.setAttribute("bbj-remove", "true");

    String name = getComponentTagName();
    if (name.length() > 0 && name.contains("-")) {
      hv.setAttribute(name, "");
    }
  }

  /**
   * Get the UUID of the component.
   * 
   * The UUID is used to identify the component in the DOM and to
   * communicate with it from the server. The UUID is generated
   * automatically and cannot be changed.
   * 
   * @return the UUID of the component.
   */
  public String getUUID() {
    return uuid;
  }

  /**
   * Check if the web component is attached to a panel.
   * 
   * @return true if the web component is attached to a panel and not destroyed,
   *         false otherwise
   */
  public boolean isAttached() {
    return panel != null && !isDestroyed();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void destroy() {
    if (isDestroyed()) {
      return;
    }

    getHtmlContainer().destroy();
    properties.clear();
    attributes.clear();
    asyncScripts.clear();
    registeredClientEvents.clear();
    clientEventMap.clear();
    slots.clear();
    rawSlots.clear();
    dispatcher.clear();

    super.destroy();
  }

  /**
   * A hook that is called when the web component is attached to a panel and
   * directly before any scripts are injected.
   * 
   * Subclasses can override this method to perform any initialization tasks.
   * 
   * @param panel the panel that the web component is attached to
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void onAttach(AbstractPanel panel) {
    assertNotDestroyed();
  }

  /**
   * A hook that is is called when the web component is attached to the panel
   * and directly after all scripts are injected.
   * 
   * @param panel the panel that the web component is detached from
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void onFlush(AbstractPanel panel) {
    assertNotDestroyed();

    for (Map.Entry<String, Entry<AbstractPanel, Boolean>> entry : slots.entrySet()) {
      AbstractPanel slotPanel = entry.getValue().getKey();
      if (slotPanel != null) {
        slotPanel.setVisible(true);
      }
    }
  }

  /**
   * Get the panel that the web component is attached to or null if the web
   * component is not attached to any panel.
   * 
   * @return the panel instance or null if the web component is not attached to
   *         any panel or destroyed
   */
  protected AbstractPanel getPanel() {
    if (!isDestroyed()) {
      return panel;
    }

    return null;
  }

  /**
   * Get the events dispatcher of the web component.
   * 
   * @return the event dispatcher
   * @See {@link EventDispatcher}
   */
  protected EventDispatcher getEventDispatcher() {
    return dispatcher;
  }

  /**
   * Get the HTML container instance.
   * 
   * @return the HTML container or null if the web component is destroyed
   */
  protected HtmlContainer getHtmlContainer() {
    if (!isDestroyed()) {
      return hv;
    }

    return null;
  }

  /**
   * Get the tag name of the web component.
   * 
   * Web component tag name is the defined in the {@link NodeName} annotation.
   * 
   * @return the tag name of the web component
   * @throws DwcjRuntimeException if the web component class is not annotated
   *                             with @NodeName
   */
  protected String getComponentTagName() {
    if (getClass().isAnnotationPresent(NodeName.class)) {
      return getClass().getAnnotation(NodeName.class).value();
    } else {
      throw new DwcjRuntimeException(
          "The web component class must be annotated with @NodeName");
    }
  }

  /**
   * Get the default html view of the web component.
   * 
   * @return the default html view of the web component or empty string if the web
   *         component is destroyed
   * 
   * @throws DwcjRuntimeException if the web component class is not annotated
   *                             with @NodeName
   */
  protected String getView() {
    if (isDestroyed()) {
      return "";
    }

    String name = getComponentTagName();

    // parse NodeClass annotations
    NodeClassName[] classes = getClass().getAnnotationsByType(NodeClassName.class);
    ArrayList<String> args = new ArrayList<>();
    // value is an array of strings
    for (NodeClassName c : classes) {
      args.addAll(Arrays.asList(c.value()));
    }

    if (!args.isEmpty()) {
      addComponentClassName(args.toArray(new String[args.size()]));
    }

    // parse NodeProperty annotations
    NodeProperty[] props = getClass().getAnnotationsByType(NodeProperty.class);
    for (NodeProperty p : props) {
      setComponentProperty(p.name(), p.value());
    }

    // tag name is empty, the we use the the html container as the root element
    if (name.trim().length() == 0) {
      hv.setAttribute("dwcj-wc", getUUID());

      NodeAttribute[] attrs = getClass().getAnnotationsByType(NodeAttribute.class);
      for (NodeAttribute a : attrs) {
        hv.setAttribute(a.name(), a.value());
      }

      return "";
    }
    // if the tag name is defined then we use as root element
    else {
      // parse NodeAttribute annotations
      NodeAttribute[] attrs = getClass().getAnnotationsByType(NodeAttribute.class);
      StringBuilder attr = new StringBuilder();
      for (NodeAttribute a : attrs) {
        attr.append(" ").append(a.name()).append("=\"").append(a.value()).append("\"");
      }

      attr.append(" dwcj-wc=\"").append(getUUID()).append("\"");

      StringBuilder view = new StringBuilder();
      view.append("<").append(name).append(attr).append(">")
          .append("</").append(name).append(">");

      return view.toString();
    }
  }

  /**
   * Invoke a client method on the web component asynchronously.
   * 
   * The method is invoked asynchronously on the client and the result is
   * discarded. In other words, no response is expected from the client.
   * 
   * The method name can be: An actual method name, "This" or "Exp".
   * 
   * <ul>
   * <li>
   * <b>Method name</b> - The name of a method on the web component.
   * for example:
   * 
   * <pre>
   * {@code
   * invokeAsync("myComponentMethod", "arg1", "arg2");
   * }
   * </pre>
   * 
   * <li>
   * <b>This</b> - The web component instance.
   * The method can be use to set a property on the web component.
   * for example:
   * 
   * <pre>
   * {@code
   * invokeAsync("this", "value", "new value");
   * }
   * </pre>
   * 
   * </li>
   * 
   * <li>
   * <b>Exp</b> - A javascript expression that will be wrapped in a
   * function and invoked on the client.
   * 
   * An expression is a string that is evaluated by the javascript engine.
   * Every expression have access to the web component instance via the
   * "component" variable.
   * 
   * When working with expressions keep the following points in mind:
   * 
   * <ul>
   * <li>Expressions are evaluated in the context of the web component
   * instance
   * <li>If the expression must return a value and has the word return in it, then
   * we will assume it is a multi-line expression and will not wrap it.</li>
   * <li>If the expression must return a value and does not have the word return
   * in it, then we will insert the return statement and the ";".</li>
   * <li>If the expression has many lines, then you will need to provide the ";""
   * at the end of each line and also provide the return statement.</li>
   * </ul>
   * 
   * For example:
   * 
   * <pre>
   * {@code
   * invokeAsync("Function", "alert(component.value)");
   * }
   * </pre>
   * </ul>
   * 
   * @param method the method name
   * @param args   the method arguments
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void invokeAsync(String method, Object... args) {
    doInvoke(true, method, args);
  }

  /**
   * Alias for {@link #invokeAsync(String, Object...)}
   * 
   * @param method the method name
   * @param args   the method arguments
   * 
   * @return The web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void callAsyncFunction(String functionName, Object... args) {
    invokeAsync(functionName, args);
  }

  /**
   * Execute a javascript expression asynchronously.
   * 
   * @param expression the expression to execute
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #invokeAsync(String, Object...)
   */
  protected void executeAsyncExpression(String expression) {
    invokeAsync("Exp", expression);
  }

  /**
   * Invoke a client method on the web component synchronously.
   * 
   * The method is invoked synchronously on the client and the result is
   * returned. In other words, a response is expected from the client.
   * 
   * The method name can be: An actual method name, "This" or "Exp".
   * 
   * <ul>
   * <li>
   * <b>Method name</b> - The name of a method on the web component.
   * for example:
   * 
   * <pre>
   * {@code
   * String value = invoke("myComponentMethod", "arg1", "arg2");
   * }
   * </pre>
   * 
   * <li>
   * <b>This</b> - The web component instance.
   * Depending on the number of arguments, the method can be used to get a
   * property or set a property on the web component.
   * for example:
   * 
   * <pre>
   * {@code
   * String value = invoke("this", "value");
   * invoke("this", "value", "new value");
   * }
   * </pre>
   * 
   * </li>
   * 
   * <li>
   * <b>Exp</b> - A javascript expression that will be wrapped in a
   * function and invoked on the client and the result is returned.
   * 
   * An expression is a string that is evaluated by the javascript engine.
   * Every expression have access to the web component instance via the
   * "component" variable.
   * 
   * When working with expressions keep the following points in mind:
   * <ul>
   * <li>Expressions are evaluated in the context of the web component
   * instance
   * <li>If the expression must return a value and has the word return in it, then
   * we will assume it is a multi-line expression and will not wrap it.</li>
   * <li>If the expression must return a value and does not have the word return
   * in it, then we will insert the return statement and the ";".</li>
   * <li>If the expression has many lines, then you will need to provide the ";""
   * at the end of each line and also provide the return statement.</li>
   * </ul>
   * 
   * For example:
   * 
   * <pre>
   * {@code
   * String value = invoke("Exp", "component.value");
   * }
   * </pre>
   * </ul>
   * 
   * @param method the method name
   * @param args   the method arguments
   * 
   * @return The result of the method
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected Object invoke(String method, Object... args) {
    return doInvoke(false, method, args);
  }

  /**
   * Alias for {@link #invoke(String, Object...)}
   * 
   * @param method the method name
   * @param args   the method arguments
   * 
   * @return The result of the method
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected Object callFunction(String functionName, Object... args) {
    return invoke(functionName, args);
  }

  /**
   * Execute a javascript expression.
   * 
   * @param expression the expression to execute
   * 
   * @return the result of the expression
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #invoke(String, Object...)
   */
  protected Object executeExpression(String expression) {
    return invoke("Exp", expression);
  }

  /**
   * Add an event listener.
   * 
   * Event listeners are instances of {@link EventListener} that are invoked when
   * an event is fired on the web component.
   * 
   * Every component must define the events it fires. This is done by annotating
   * the supported event classes with {@link EventName}.
   * Further control over the event data can be done by annotating the event class
   * with {@link EventExpressions}.
   * 
   * @param <K>        the event class
   * @param eventClass the event class
   * @param listener   the event listener
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @throws DwcjRuntimeException if the event class is not annotated
   *                             with @EventName
   */
  protected <K extends Event<?>> void addEventListener(
      Class<K> eventClass,
      EventListener<K> listener) {
    assertNotDestroyed();

    String eventName = getEventName(eventClass);
    dispatcher.addEventListener(eventClass, listener);
    clientEventMap.put(eventName, eventClass);

    // register the event on the client side
    if (!registeredClientEvents.contains(eventName)) {
      StringBuilder exp = new StringBuilder();
      exp.append("Dwcj.WcConnector.addEventListener('")
          .append(getUUID()).append("', '")
          .append(eventName).append("', ");

      JsonObject options = new JsonObject();
      if (eventClass.isAnnotationPresent(EventExpressions.class)) {
        EventExpressions eventConfig = eventClass.getAnnotation(EventExpressions.class);
        options.addProperty("filter", encode.apply(eventConfig.filter()));
        options.addProperty("detail", encode.apply(eventConfig.detail()));
        options.addProperty("preventDefault", encode.apply(eventConfig.preventDefault()));
        options.addProperty("stopPropagation", encode.apply(eventConfig.stopPropagation()));
        options.addProperty("stopImmediatePropagation", encode.apply(eventConfig.stopImmediatePropagation()));
      }

      exp.append(options.toString()).append(");");
      executeAsyncExpression(exp.toString());

      registeredClientEvents.add(eventName);
    }
  }

  /**
   * Remove an event listener.
   * 
   * @param <K>        the event class
   * @param eventClass the event class
   * @param listener   the event listener
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @throws DwcjRuntimeException if the event class is not annotated
   *                             with @EventName
   */
  protected <K extends Event<?>> void removeEventListener(Class<K> eventClass,
      EventListener<K> listener) {
    assertNotDestroyed();

    String eventName = getEventName(eventClass);

    clientEventMap.remove(eventName);
    registeredClientEvents.remove(eventName);
    dispatcher.removeEventListener(eventClass, listener);

    boolean shouldReachClient = dispatcher.getListenersCount(eventClass) == 0;

    if (shouldReachClient) {
      String exp = "Dwcj.WcConnector.removeEventListener('" + getUUID() + "', '" + eventName + "');";
      executeAsyncExpression(exp);
    }
  }

  /**
   * Get the added control with the given uuid
   * 
   * @param uuid the uuid of the control to get
   * @return the control
   */
  protected AbstractComponent getControl(String uuid) {
    return controls.entrySet().stream()
        .filter(e -> e.getValue().getKey().equals(uuid))
        .map(e -> e.getKey())
        .findFirst()
        .orElse(null);
  }

  /**
   * Add a control inside the web component.
   * 
   * The method will move the given DWCJ control from its current parent in the
   * DOM tree to the web component.
   * 
   * <p>
   * Note if the control is not attached to any dwcj panel yet, the method will
   * attach it to the panel of the web component when the component is attached.
   * </p>
   * 
   * @param control the control to add
   * @return the uuid of the control
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the control is null, the control is the
   *                                  web component itself or the control is
   *                                  destroyed.
   */
  protected String addControl(AbstractComponent control) {
    assertNotDestroyed();

    if (control == null) {
      throw new IllegalArgumentException("Cannot add a null as a control");
    }

    if (control.equals(this)) {
      throw new IllegalArgumentException("Cannot add a web component to itself");
    }

    if (control.isDestroyed()) {
      throw new IllegalArgumentException("Cannot add a destroyed control");
    }

    if (controls.containsKey(control)) {
      return controls.get(control).getKey();
    }

    // assign a uuid to the control
    String newUuid = UUID.randomUUID().toString().substring(0, 8);
    controls.put(control, new SimpleEntry<>(newUuid, false));

    // add dwcj-ctrl attribute to the control to link it to the web component
    if (control instanceof AbstractDwcComponent) {
      ((AbstractDwcComponent) control).setAttribute("dwcj-ctrl", newUuid);
    }

    if (control instanceof WebComponent) {
      MethodHandle method;
      try {
        // look up the getHtmlContainer method with MethodHandles
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        MethodType mt = MethodType.methodType(HtmlContainer.class);
        method = lookup.findVirtual(control.getClass(), "getHtmlContainer", mt);
        HtmlContainer container = (HtmlContainer) method.invoke(control);
        container.setAttribute("dwcj-ctrl", newUuid);
      } catch (Throwable e) {
        // pass
        Environment.logError("Failed to set web component attribute. " + e.getMessage());
      }
    }

    // attach the control to the webcomponent's panel
    if (isAttached()) {
      getPanel().add(control);
      // mark as attached to the panel
      controls.get(control).setValue(true);
    }

    // move the control to the web component in the client side
    StringBuilder js = new StringBuilder();
    js.append("const selector='[dwcj-ctrl=\"").append(newUuid).append("\"]';")
        .append("const control = document.querySelector(selector);")
        .append("if(control)")
        .append(" component.appendChild(control);")
        .append("return;"); // avoid auto wrapping

    executeAsyncExpression(js.toString());

    return newUuid;
  }

  /**
   * Remove a control from the web component.
   * 
   * The method will remove the given DWCJ control from the web component
   * and destroy it.
   * 
   * @param String the uuid of the control to remove
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void removeControl(String uuid) {
    assertNotDestroyed();

    if (uuid != null) {
      AbstractComponent control = getControl(uuid);
      if (control != null) {
        controls.remove(control);
        control.destroy();
      }
    }
  }

  /**
   * Get the raw slot value (the html content).
   * 
   * @param slot the slot name
   * @return the raw slot value if the web component is not destroyed, an empty
   *         string otherwise
   */
  protected String getRawSlot(String slot) {
    if (isDestroyed()) {
      return "";
    }

    return rawSlots.get(slot);
  }

  /**
   * Get the default raw slot value (the html content).
   * 
   * @return the raw slot value if the web component is not destroyed, an empty
   *         string otherwise.
   */
  protected String getRawSlot() {
    return getRawSlot("__EMPTY_SLOT__");
  }

  /**
   * Set the raw slot value (the html content).
   * 
   * The method will use the given html content and render it in the web
   * component defined slot.
   * 
   * The method can be invoked multiple times to update the slot content.
   * 
   * @param slot  the slot name
   * @param value the raw slot value
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a slot
   */
  protected void addRawSlot(String slot, String value) {
    assertNotDestroyed();

    if (slots.containsKey(slot)) {
      throw new IllegalArgumentException("The slot " + slot + " is already defined as a slot");
    }

    rawSlots.put(slot, value);

    String selector = "";
    if (!slot.equals("__EMPTY_SLOT__")) {
      selector += "[slot='" + slot + "'][dwcj-slot='" + getUUID() + "']";
    } else {
      selector += "[dwcj-dslot='true'][dwcj-slot='" + getUUID() + "']";
    }

    // add the slot to the DOM
    StringBuilder js = new StringBuilder();

    // check if the component has a span node with dwcj-${slot} attribute
    js.append("var span = component.querySelector(\"" + selector + "\"); ")
        // if the component has a span node with dwcj-${slot} attribute
        .append("if (span) {")
        // replace the text of the span node
        .append("span.innerHTML = \\`").append(value).append("\\`; ")
        .append("} else {")
        // if the component does not have a span node with dwcj-${slot} attribute
        // create a new span node and append it to the component
        .append("span = document.createElement('span');")
        .append("span.setAttribute('dwcj-slot', '" + getUUID() + "');");

    if (slot.equals("__EMPTY_SLOT__")) {
      js.append("span.setAttribute('dwcj-dslot', 'true');");
    } else {
      js.append("span.setAttribute('slot', '" + slot + "');");
    }

    js.append("span.innerHTML = \\`").append(value).append("\\`; ")
        .append("component.appendChild(span);")
        .append("}")
        .append("return '';"); // to avoid auto wrapping

    executeAsyncExpression(js.toString());
  }

  /**
   * Remove a raw slot.
   * 
   * The method will remove the given slot from the web component.
   * 
   * @param slot the slot name
   * @param html the html content
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a slot
   */
  protected void removeRawSlot(String slot) {
    assertNotDestroyed();

    if (slots.containsKey(slot)) {
      throw new IllegalArgumentException("The slot " + slot + " is already defined as a slot");
    }

    if (rawSlots.containsKey(slot)) {
      rawSlots.remove(slot);

      // attach the panel in the client side
      String selector = "";
      if (!slot.equals("__EMPTY_SLOT__")) {
        selector += "[slot='" + slot + "'][dwcj-slot='" + getUUID() + "']";
      } else {
        selector += "[dwcj-dslot='true'][dwcj-slot='" + getUUID() + "']";
      }

      // remove the slot from the DOM
      StringBuilder js = new StringBuilder();
      // check if the component has a span node with dwcj-${slot} attribute
      js.append("var span = component.querySelector(\"" + selector + "\"); ")
          // if the component has a span node with dwcj-${slot} attribute
          .append("if(span){")
          // remove the span node
          .append("span.remove();")
          .append("}")
          .append("return '';"); // to avoid auto wrapping

      executeAsyncExpression(js.toString());
    }
  }

  /**
   * Remove the default raw slot.
   * 
   * The method will remove the default slot from the web component.
   * 
   * @param html the html content
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a slot
   */
  protected void removeRawSlot() {
    removeRawSlot("__EMPTY_SLOT__");
  }

  /**
   * Set a default raw slot value (the html content).
   * 
   * @param value the raw slot value
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a slot
   * @see #addRawSlot(String, String)
   */
  protected void addRawSlot(String value) {
    addRawSlot("__EMPTY_SLOT__", value);
  }

  /**
   * Get the slot panel.
   * 
   * @param slot the slot name
   * 
   * @return the slot panel
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected AbstractPanel getSlot(String slot) {
    if (isDestroyed()) {
      return null;
    }

    if (slots.containsKey(slot)) {
      return slots.get(slot).getKey();
    }

    return null;
  }

  /**
   * Get the default slot panel
   * 
   * @return the default slot panel
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected AbstractPanel getSlot() {
    return getSlot("__EMPTY_SLOT__");
  }

  /**
   * Attach a slot to the web component.
   * 
   * If the slot is already assigned to another panel, the old panel will be
   * detached from the web component. If the slot is already assigned to the given
   * panel, the method will do nothing. If the slot is not assigned to any panel,
   * the method will attach the given panel to the web component in the given
   * slot.
   * 
   * If the given panel is not attached to any parent panel, the method will
   * attach
   * the panel to the panel of the web component where the web component is
   * attached.
   * 
   * @param slot    the slot name
   * @param panel   the panel to attach
   * @param destroy if true, and the slot is assigned to another panel, the old
   *                panel will be destroyed (removed from the DOM)
   * 
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   */
  protected void addSlot(String slot, AbstractPanel panel, boolean destroy) {
    assertNotDestroyed();

    if (rawSlots.containsKey(slot)) {
      throw new IllegalArgumentException("The slot " + slot + " is already defined as a raw slot");
    }

    if (panel.isDestroyed()) {
      throw new IllegalArgumentException("Unable to attach a destroyed panel to the slot [" + slot + "]");
    }

    if (slots.containsKey(slot)) {
      Entry<AbstractPanel, Boolean> entry = slots.get(slot);
      AbstractPanel oldPanel = entry.getKey();

      // if the new panel is different from the old one, detach the old one
      if (!oldPanel.equals(panel)) {
        removeSlot(slot);
      }
    }

    // Start tracking the slot
    slots.put(slot, new SimpleEntry<>(panel, false));

    // bbj-remove is a special attribute in DWC.
    // If the child element isn't contained in the specified parent, go ahead and
    // remove it anyway if either the child or actual parent contains the attribute
    // "bbj-remove".
    panel.setAttribute("bbj-remove", "true");

    // mark the panel with the slot name and the web component uuid
    // to be able to find it in the client side
    panel.setAttribute("dwcj-slot", getUUID());
    if (!slot.equals("__EMPTY_SLOT__")) {
      // assign the slot name to the panel
      panel.setAttribute("slot", slot);
    } else {
      // mark the panel as default slot
      panel.setAttribute("dwcj-dslot", "true");
    }

    // attach the panel directly if the web component is already attached
    if (isAttached()) {
      getPanel().add(panel);
      slots.get(slot).setValue(true);
    } else {
      // hide the panel until the web component is attached
      panel.setVisible(false);
    }

    // attach the panel in the client side
    String selector = "";
    if (!slot.equals("__EMPTY_SLOT__")) {
      selector += "[slot='" + slot + "'][dwcj-slot='" + getUUID() + "']";
    } else {
      selector += "[dwcj-dslot='true'][dwcj-slot='" + getUUID() + "']";
    }

    executeAsyncExpression("component.appendChild(document.querySelector(\"" + selector + "\"));");
  }

  /**
   * Attach a slot to the web component.
   * 
   * @param slot  the slot name
   * @param panel the panel to attach
   * 
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   * @see #addSlot(String, AbstractPanel, boolean)
   */
  protected void addSlot(String slot, AbstractPanel panel) {
    addSlot(slot, panel, true);
  }

  /**
   * Attach the default slot to the web component.
   * 
   * @param panel the panel to attach
   * 
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   * @see #addSlot(String, AbstractPanel, boolean)
   */
  protected void addSlot(AbstractPanel panel) {
    addSlot("__EMPTY_SLOT__", panel);
  }

  /**
   * Detach a slot from the web component.
   * 
   * The method will detach the panel from the web component and then it is up to
   * developer to destroy it later.
   * 
   * @param slot    the slot name
   * @param destroy if true, the panel will be destroyed (removed from the DOM)
   *                if false , the panel will be hidden without removing it from
   *                the DOM and then it is up to developer to destroy it later.
   * 
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   */
  protected void removeSlot(String slot, boolean destroy) {
    assertNotDestroyed();

    if (rawSlots.containsKey(slot)) {
      throw new IllegalArgumentException("The slot " + slot + " is already defined as a raw slot");
    }

    if (slots.containsKey(slot)) {
      Entry<AbstractPanel, Boolean> entry = slots.get(slot);
      AbstractPanel panelToRemove = entry.getKey();

      // remove the slot attributes from the panel
      // Currently, BBjControl.removeAttributes() is not implemented
      // so we just change the attribute values without reaching the DOM
      // TODO: reimplement this when BBjControl.removeAttributes() is implemented
      // Even though if the panel is destroyed. we should remove the attributes
      if (!slot.equals("__EMPTY_SLOT__")) {
        panelToRemove.setAttribute("slot", "__detached__");
      } else {
        panelToRemove.setAttribute("dwcj-dslot", "false");
      }

      // detach the panel from the web component
      slots.remove(slot);
      if (destroy) {
        panelToRemove.destroy();
      } else {
        panelToRemove.setVisible(false);
      }
    }
  }

  /**
   * Detach a slot from the web component.
   * 
   * @param slot the slot name
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   * @see #removeSlot(String, boolean)
   */
  protected void removeSlot(String slot) {
    removeSlot(slot, true);
  }

  /**
   * Detach the default slot from the web component.
   * 
   * @return the web component
   * @throws ComponentDestroyed      if the web component is destroyed
   * @throws IllegalArgumentException if the slot is already defined as a raw slot
   * @see #removeSlot(String, boolean)
   */
  protected void removeSlot() {
    removeSlot("__EMPTY_SLOT__");
  }

  /**
   * Get a component attribute.
   * 
   * @param name         the name of the attribute
   * @param defaultValue the default value of the attribute
   * @param fromClient   true if the attribute should be read from the client
   * 
   * @return the value of the attribute or the default value if the attribute is
   *         not set or the web component is destroyed
   */
  protected String getComponentAttribute(String name, String defaultValue, boolean fromClient) {
    if (isDestroyed()) {
      return defaultValue;
    }

    Object result = null;

    if (fromClient) {
      // try to get the attribute from the client
      // if the attribute is not set on the client or the client is not connected
      // the default value will be returned
      result = invoke("getAttribute", name);
      if (result == null) {
        return defaultValue;
      }
    } else {
      // try to get the attribute from the server
      // if the attribute is not set on the server the default value will be returned
      result = attributes.get(name);
      if (result == null) {
        return defaultValue;
      }
    }

    return result.toString();
  }

  /**
   * Get component attribute from the server
   * 
   * @param name         the name of the attribute
   * @param defaultValue the default value of the attribute
   * 
   * @return the value of the attribute or the default value if the attribute is
   *         not set or the web component is destroyed.
   * @see #getComponentAttribute(String, String, boolean)
   */
  protected String getComponentAttribute(String name, String defaultValue) {
    return getComponentAttribute(name, defaultValue, false);
  }

  /**
   * Get component attribute from the client
   * 
   * @param name the name of the attribute
   * 
   * @return the value of the attribute or null if the attribute is not set or the
   *         web component is destroyed.
   * @see #getComponentAttribute(String, String, boolean)
   */
  protected String getComponentAttribute(String name) {
    return getComponentAttribute(name, null, true);
  }

  /**
   * Set an attribute of the web component
   * 
   * @param name  the name of the attribute
   * @param value the value of the attribute
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #setComponentAttribute(String, String, boolean)
   */
  protected void setComponentAttribute(String name, String value) {
    invokeAsync("setAttribute", name, value);
    attributes.put(name, value);
  }

  /**
   * Set an attribute of the web component
   * 
   * @param name the name and the value of the attribute
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #setComponentAttribute(String, String)
   */
  protected void setComponentAttribute(String name) {
    setComponentAttribute(name, name);
  }

  /**
   * Remove an attribute of the web component
   * 
   * @param name the name of the attribute
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void removeComponentAttribute(String name) {
    invokeAsync("removeAttribute", name);
    attributes.remove(name);
  }

  /**
   * Get a property of the web component.
   * 
   * @param name         the name of the property
   * @param defaultValue the default value of the property
   * @param fromClient   true if the property should be read from the client
   * 
   * @return the value of the property or the default value if the property is not
   *         set or the web component is destroyed.
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected Object getComponentProperty(String name, Object defaultValue, boolean fromClient) {
    if (isDestroyed()) {
      return defaultValue;
    }

    Object result = null;

    if (fromClient) {
      // try to get the property from the client
      // if the property is not set on the client or the client is not connected
      // the default value will be returned
      result = invoke("this", name);
      if (result == null) {
        return defaultValue;
      }
    } else {
      // try to get the property from the server
      // if the property is not set on the server the default value will be returned
      result = properties.get(name);
      if (result == null) {
        return defaultValue;
      }
    }

    return result;
  }

  /**
   * Get a property of the web component from the server.
   * 
   * @param name         the name of the property
   * @param defaultValue the default value of the property
   * 
   * @return the value of the property or the default value if the property is not
   *         set or the web component is destroyed.
   * @see #getComponentProperty(String, Object, boolean)
   */
  protected Object getComponentProperty(String name, Object defaultValue) {
    return getComponentProperty(name, defaultValue, false);
  }

  /**
   * Get a property of the web component from the server.
   * 
   * @param name the name of the property
   * 
   * @return the value of the property or null if the property is not set or the
   *         web component is destroyed.
   * @see #getComponentProperty(String, Object, boolean)
   */
  protected Object getComponentProperty(String name) {
    return getComponentProperty(name, null, false);
  }

  /**
   * Set a property of the web component.
   * 
   * @param name  the name of the property
   * @param value the value of the property
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #setComponentProperty(String, Object, boolean)
   */
  protected void setComponentProperty(String name, Object value) {
    invokeAsync("this", name, value);
    properties.put(name, value);
  }

  /**
   * Set a property of the web component.
   * 
   * @param name the name and the value of the property
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #setComponentProperty(String, Object)
   */
  protected void setComponentProperty(String name) {
    setComponentProperty(name, name);
  }

  /**
   * Get a property or an attribute of the web component.
   * 
   * @param <V>        the type of the property
   * @param property   the property
   * @param fromClient true if the property should be read from the client
   * @param type       the type of the property
   * 
   * @return the value of the property or attribute or the default value if the
   *         property or attribute is not set or the web component is destroyed.
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #getComponentProperty(String, Object, boolean)
   * @see #getComponentAttribute(String, String, boolean)
   */
  protected <V> V get(PropertyDescriptor<V> property, boolean fromClient, Type type) {
    if (isDestroyed()) {
      return property.getDefaultValue();
    }

    boolean isAttribute = property.isAttribute();

    if (!isAttribute) {
      if (fromClient) {
        // we need to convert the json string to the correct type.
        String json = String.valueOf(getComponentProperty(property.getName(), property.getDefaultValue(), fromClient));
        if (json == null) {
          return property.getDefaultValue();
        }

        // convert object to the correct type
        V result = new Gson().fromJson(json, type);
        return result;
      } else {
        @SuppressWarnings("unchecked")
        V result = (V) getComponentProperty(property.getName(), property.getDefaultValue(), fromClient);
        return result;
      }
    } else {
      @SuppressWarnings("unchecked")
      V result = (V) getComponentAttribute(property.getName(), String.valueOf(property.getDefaultValue()), fromClient);
      return result;
    }
  }

  /**
   * Get a property or an attribute of the web component.
   * 
   * @param <V>      the type of the property
   * @param property the property
   * 
   * @return the value of the property or attribute or the default value if the
   *         property or attribute is not set or the web component is destroyed.
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #get(PropertyDescriptor, boolean, Type)
   */
  protected <V> V get(PropertyDescriptor<V> property) {
    return get(property, false, null);
  }

  /**
   * Set a property or an attribute of the web component.
   * 
   * @param <V>      the type of the property
   * @param property the property
   * @param value    the value of the property
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #setComponentProperty(String, Object)
   * @see #setComponentAttribute(String, String)
   */
  protected <V> void set(PropertyDescriptor<V> property, V value) {
    assertNotDestroyed();

    boolean isAttribute = property.isAttribute();

    if (!isAttribute) {
      setComponentProperty(property.getName(), value);
    } else {
      setComponentAttribute(property.getName(), value.toString());
    }
  }

  /**
   * Set a property or an attribute of the web component.
   * 
   * @param <V>      the type of the property
   * @param property the property
   * 
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   * @see #set(PropertyDescriptor, Object)
   */
  protected <V> void set(PropertyDescriptor<V> property) {
    set(property, property.getDefaultValue());
  }

  /**
   * Add a class name to the web component
   * 
   * @param className the class name
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void addComponentClassName(String... className) {
    invokeAsync("classList.add", (Object[]) className);
  }

  /**
   * Remove a class name from the web component
   * 
   * @param className the class name
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void removeComponentClassName(String... className) {
    invokeAsync("classList.remove", (Object[]) className);
  }

  /**
   * Set a style of the web component
   * 
   * @param name  the name of the style
   * @param value the value of the style
   * @return the web component
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void setComponentStyle(String name, String value) {
    invokeAsync("style.setProperty", name, value);
  }

  /**
   * Get a style of the web component
   * 
   * @param name the name of the style
   * @return the value of the style
   */
  protected String getComponentStyle(String name) {
    Object result = executeExpression("style.getPropertyValue(\\'" + name + "\\')");
    return String.valueOf(result);
  }
  
  /**
   * Get the computed style of the web component
   * 
   * @param name the name of the style
   * @return the computed value of the style
   */
  protected String getComponentComputedStyle(String name) {
    Object result = executeExpression("window.getComputedStyle(component).getPropertyValue(\\'" + name + "\\')");
    return String.valueOf(result);
  }

  /**
   * Remove a style of the web component
   * 
   * @param name the name of the style
   * @return the web component
   * 
   * @throws ComponentDestroyed if the web component is destroyed
   */
  protected void removeComponentStyle(String name) {
    invokeAsync("style.removeProperty", name);
  }

  /**
   * Create the control.
   * 
   * This method is called by the framework when the control is added to a panel.
   * You should not call this method directly.
   * 
   * @param panel the parent panel
   * @throws ComponentDestroyed if the web component is destroyed
   */
  @Override
  protected void create(AbstractPanel panel) {
    assertNotDestroyed();

    if (isAttached()) {
      return;
    }

    // parse HtmlViewAttribute annotations
    HtmlViewAttribute[] attrs = getClass().getAnnotationsByType(HtmlViewAttribute.class);
    for (HtmlViewAttribute attr : attrs) {
      hv.setAttribute(attr.name(), attr.value());
    }

    // parse HtmlViewClass annotations
    HtmlViewClassName[] classes = getClass().getAnnotationsByType(HtmlViewClassName.class);
    // value is an array of strings
    for (HtmlViewClassName c : classes) {
      for (String s : c.value()) {
        hv.addClassName(s);
      }
    }

    // attach the stylesheets
    String key = "org.dwcj.WebComponent::styles";
    boolean attached = ObjectTable.contains(key);
    if (!attached) {
      App.getPage().addInlineStyleSheet(getStylesheets(), true, "id=wc-styles");
      ObjectTable.put(key, true);
    }

    // attach the javascript
    key = "org.dwcj.WebComponent::scripts";
    attached = ObjectTable.contains(key);
    if (!attached) {
      App.getPage().addInlineJavaScript("context://webcompoent/wcconnector.min.js", true, "id=wc-scripts");
      ObjectTable.put(key, true);
    }

    this.panel = panel;

    hv.setText(getView());
    panel.add(hv);

    // loop over the slots and add them to the web component panel
    for (Map.Entry<String, Entry<AbstractPanel, Boolean>> entry : slots.entrySet()) {
      AbstractPanel slotPanel = entry.getValue().getKey();
      boolean isAttached = entry.getValue().getValue();

      if (slotPanel != null && !isAttached) {
        panel.add(slotPanel);
      }
    }

    // loop over the controls and add them to the web component panel
    for (Entry<AbstractComponent, Entry<String, Boolean>> entry : controls.entrySet()) {
      AbstractComponent control = entry.getKey();
      boolean isAttached = entry.getValue().getValue();

      if (control != null && !isAttached) {
        panel.add(control);
      }
    }

    onAttach(panel);

    // execute scripts asynchronously
    for (String script : asyncScripts) {
      hv.executeAsyncScript(script);
    }

    onFlush(panel);
  }

  /**
   * Invoke a method of the web component.
   * 
   * @param async  true if the method is async
   * @param method the method
   * @param args   the arguments
   * 
   * @return the result
   * @throws ComponentDestroyed if the web component is destroyed
   */
  private Object doInvoke(boolean async, String method, Object... args) {
    assertNotDestroyed();

    // TODO: Ask Jim to add support for async calls using executeScript
    StringBuilder js = new StringBuilder();
    js.append("Dwcj.WcConnector.invoke('").append(getUUID()).append("',");

    String methodLower = method.toLowerCase();

    // set or get property
    if (methodLower.equals("this")) {
      js.append("'this',");
      int len = args.length;
      if (len == 1) {
        // get property
        js.append(String.valueOf(args[0]));
      } else if (len == 2) {
        // set property
        js.append("'" + args[0] + "',").append(new Gson().toJson(args[1]));
      }
    }

    // execute an expression
    else if (methodLower.equals("exp")) {
      js.append("'exp',")
          .append("'" + encode.apply(String.valueOf(args[0])) + "'");
    }

    // invoke a method on the component
    else {
      js.append("'" + method + "'");
      if (args.length > 0) {
        js.append(",");
      }

      // add arguments
      for (int i = 0; i < args.length; i++) {
        if (i > 0) {
          js.append(", ");
        }

        if (args[i] instanceof WebComponent.JsRawParam) {
          js.append(String.valueOf(args[i]));
        } else {
          js.append(new Gson().toJson(args[i]));
        }
      }
    }

    js.append(")"); // end of Dwcj.WcConnector.invoke
    if (hv.getCaughtUp()) {
      if (async) {
        hv.executeAsyncScript(js.toString());
      } else {
        return hv.executeScript(js.toString());
      }
    } else {
      asyncScripts.add(js.toString());
    }

    return null;
  }

  /**
   * Handle javascript events
   * 
   * @param htmlContainerJavascriptEvent
   */
  private void handleJavascriptEvents(HtmlContainerJavascriptEvent htmlContainerJavascriptEvent) {
    if (isDestroyed()) {
      return;
    }

    Map<String, String> eventMap = htmlContainerJavascriptEvent.getEventMap();
    // the name of the server side event
    String type = (String) eventMap.get("type");

    // fire a custom event
    EventDispatcher eventDispatcher = getEventDispatcher();
    if (eventDispatcher != null && clientEventMap.containsKey(type)) {
      String detail = (String) eventMap.get("detail");
      Map<String, Object> data = new Gson().fromJson(
          detail,
          new TypeToken<Map<String, Object>>() {
          }.getType());
      Event<?> event = createEvent(clientEventMap.get(type), data);

      if (event != null) {
        eventDispatcher.dispatchEvent(event);
      }
    }
  }

  /**
   * Get the event name for the given event class.
   * 
   * @param eventClass the event class
   * @return the event name
   * @throws DwcjRuntimeException if the event class is not annotated
   *                             with @EventName
   */
  private String getEventName(Class<? extends Event<?>> eventClass) {
    String eventName = null;

    if (eventClass.isAnnotationPresent(EventName.class)) {
      eventName = eventClass.getAnnotation(EventName.class).value();
    } else {
      throw new DwcjRuntimeException(
          "The event class must be annotated with @EventName");
    }

    return eventName;
  }

  /**
   * Create a web component event.
   * 
   * @param <E>        the type of the event
   * @param eventClass the class of the event
   * @param data       the data
   * 
   * @return the event
   */
  private <E extends Event<?>> E createEvent(Class<E> eventClass, Map<String, Object> data) {
    E event = null;

    Constructor<?>[] constructors = eventClass.getDeclaredConstructors();
    for (Constructor<?> constructor : constructors) {
      constructor.setAccessible(true);
      Class<?>[] parameterTypes = constructor.getParameterTypes();

      // is not inner class
      if (parameterTypes.length == 2 &&
          WebComponent.class.isAssignableFrom(parameterTypes[0]) &&
          parameterTypes[1] == Map.class) {
        try {
          event = (E) constructor.newInstance(this, data); // NOSONAR
          break;
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          Environment.logError("Failed to create webcomponent event", e);
        }
      }
      // else if inner class
      else if (parameterTypes.length == 3 &&
          WebComponent.class.isAssignableFrom(parameterTypes[0]) &&
          WebComponent.class.isAssignableFrom(parameterTypes[1]) &&
          parameterTypes[2] == Map.class) {
        try {
          event = (E) constructor.newInstance(this, this, data); // NOSONAR
          break;
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
            | InvocationTargetException e) {
          Environment.logError("Failed to create webcomponent event", e);
        }
      }
    }

    return event;
  }

  /**
   * Get the default css styles of the web component
   * 
   * @return the default css styles of the web component or empty string if the
   *         web component is destroyed
   */
  private String getStylesheets() {
    if (isDestroyed()) {
      return "";
    }

    return "[dwcj-slot]{overflow: visible}" +
        "[dwcj-hv]{overflow: visible}" +
        "[dwcj-hv] .BBjHtmlView-content{overflow: visible}";
  }

  /**
   * Assert that the web component is not destroyed
   */
  private void assertNotDestroyed() {
    if (isDestroyed()) {
      throw new ComponentDestroyed(
          String.format("WebComponent %s [id=%s] is destroyed", getComponentTagName(), getUUID()));
    }
  }

  /**
   * A parameter that is passed as a JavaScript expression.
   * 
   * @author Hyyan Abo Fakher
   */
  public class JsRawParam {

    private String param;

    /**
     * Construct new instance of JsExpressionParam
     * 
     * @param param the JavaScript expression
     */
    public JsRawParam(String param) {
      this.param = param;
    }

    /**
     * Get the JavaScript expression
     * 
     * @return the JavaScript expression
     */
    public String getParam() {
      return param;
    }

    /**
     * Get the JavaScript expression as string
     * 
     * @return the JavaScript expression
     */
    @Override
    public String toString() {
      return param;
    }
  }
}
