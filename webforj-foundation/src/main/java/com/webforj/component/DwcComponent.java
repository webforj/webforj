package com.webforj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.Editable;
import com.basis.bbj.proxies.sysgui.TextAlignable;
import com.basis.bbj.proxies.sysgui.TextControl;
import com.basis.startup.type.BBjException;
import com.google.gson.Gson;
import com.webforj.component.event.ComponentEventSinkRegistry;
import com.webforj.component.event.MouseEnterEvent;
import com.webforj.component.event.MouseExitEvent;
import com.webforj.component.event.RightMouseDownEvent;
import com.webforj.component.event.sink.MouseEnterEventSink;
import com.webforj.component.event.sink.MouseExitEventSink;
import com.webforj.component.event.sink.RightMouseDownEventSink;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.concern.HasAttribute;
import com.webforj.concern.HasClassName;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasHtml;
import com.webforj.concern.HasProperty;
import com.webforj.concern.HasSize;
import com.webforj.concern.HasStyle;
import com.webforj.concern.HasText;
import com.webforj.concern.HasTooltip;
import com.webforj.concern.HasVisibility;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.exceptions.WebforjRestrictedAccessException;
import com.webforj.exceptions.WebforjRuntimeException;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EventObject;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * This base class serves as the fundamental class for the majority of DWC components.
 *
 * <p>
 * All essential DWC controls are required to inherit from the DwcComponent class and provide
 * default implementations for the interface methods they use. The sole exception to this rule is
 * the {@link RadioButtonGroup} component, which relies on a core BBjRadioGroup control but does not
 * inherit from BBjControl in BBj itself.
 * </p>
 *
 * <p>
 * <strong>Warning:</strong> Custom components outside the core package should not directly inherit
 * from this class. Any attempt to do so will result in an {@link IllegalAccessException}. Custom
 * components must instead start from the base {@link Component} class or any of its subclasses.
 * </p>
 *
 * @see Component
 * @see DwcFocusableComponent
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public abstract class DwcComponent<T extends DwcComponent<T>> extends Component
    implements HasText<T>, HasHtml<T>, HasAttribute<T>, HasProperty<T>, HasClassName<T>,
    HasStyle<T>, HasVisibility<T>, HasTooltip<T>, HasSize<T> {

  private final List<String> classNames = new ArrayList<>();
  private final List<String> removedClassNames = new ArrayList<>();
  private final List<String> removedStyles = new ArrayList<>();
  private final Map<String, String> attributes = new HashMap<>();
  private final Map<String, String> styles = new HashMap<>();
  private final Map<String, Object> properties = new HashMap<>();
  private final EventDispatcher dispatcher = new EventDispatcher();
  private final ComponentEventSinkRegistry<MouseEnterEvent> mouseEnterEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new MouseEnterEventSink(this, dispatcher),
          MouseEnterEvent.class);
  private final ComponentEventSinkRegistry<MouseExitEvent> mouseExitEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new MouseExitEventSink(this, dispatcher),
          MouseExitEvent.class);
  private final ComponentEventSinkRegistry<RightMouseDownEvent> rightMouseDownEventSinkListenerRegistry =
      new ComponentEventSinkRegistry<>(new RightMouseDownEventSink(this, dispatcher),
          RightMouseDownEvent.class);

  private BBjControl control;
  private boolean readOnly = false;
  private boolean visible = true;
  private String text = "";
  private String tooltipText = "";
  private HasHighlightOnFocus.Behavior highlightOnFocus = HasHighlightOnFocus.Behavior.FOCUS_OR_KEY;
  private HasHorizontalAlignment.Alignment defaultHorizontalAlignment = null;
  private HasHorizontalAlignment.Alignment horizontalAlignment = null;
  private Enum<? extends ExpanseBase> expanse = null;
  private String placeholder = "";
  private Enum<? extends ThemeBase> theme = null;
  private String width = "";
  private String minWidth = "";
  private String maxWidth = "";
  private String height = "";
  private String minHeight = "";
  private String maxHeight = "";

  /**
   * {@inheritDoc}
   */
  @Override
  public String getClientComponentId() {
    if (isAttached()) {
      try {
        return getControl().getClientObjectID();
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to get client object ID", e);
      }
    }

    return null;
  }

  /**
   * Sets the underling BBj Control.
   *
   * @param control the BBj control to set.
   */
  protected void setControl(BBjControl control) {
    this.control = control;
  }

  /**
   * This method gets the underlying original BBj control. It's package private and can only be
   * accessed through the ControlAccessor. No API user / customer should ever work directly with BBj
   * controls.
   *
   * @return the underlying BBj control
   */
  BBjControl getControl() {
    return control;
  }

  /**
   * Sets the value for a property in the component.
   *
   * @param property the name of the property
   * @param value the value to be set
   * @return The component itself.
   *
   * @throws WebforjRestrictedAccessException if the property is restricted
   */
  @Override
  public T setProperty(String property, Object value) {
    List<String> restrictedProperties = getRestrictedProperties();
    if (!restrictedProperties.isEmpty() && restrictedProperties.contains(property)) {
      throw new WebforjRestrictedAccessException(
          "The property '" + property + "' is restricted and cannot be modified.");
    }

    return setUnrestrictedProperty(property, value);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public <V> V getProperty(String property, Type typeOfV) {
    BBjControl theControl = getControl();

    if (theControl != null) {
      try {
        return (V) theControl.getProperty(property, typeOfV);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      // convert to JSON and back to make sure we get the correct type of value as the original
      // implementation does in BBjWebComponent
      Gson gson = new Gson();
      return gson.fromJson(gson.toJson(properties.get(property)), typeOfV);
    }
  }

  /**
   * The getRestrictedProperties returns a list of properties that are restricted by the component.
   * The default implementation returns an empty ArrayList, which means that no properties are
   * restricted. Some components might need to restrict properties to prevent the API user from
   * setting properties that are supported by the component and have an already defined behavior.
   *
   * <p>
   * If a property is restricted, it also means that the corresponding attribute version of that
   * property is restricted. When converting property names to attribute names, the following
   * process is followed: CamelCase property names are separated at each capital letter, and dashes
   * are inserted between the words. For example, if the property name is "firstName", it would be
   * converted to "first-name" as an attribute.
   * </p>
   *
   * @return A list of restricted properties, or empty list if no properties are restricted.
   * @since 23.02
   */
  public List<String> getRestrictedProperties() {
    return new ArrayList<>();
  }

  /**
   * Sets the value for a specified component attribute.
   *
   * @param attribute the name of the attribute
   * @param value the value to be set
   * @return The component itself.
   *
   * @throws WebforjRestrictedAccessException if the attribute is restricted
   */
  @Override
  public T setAttribute(String attribute, String value) {
    List<String> restrictedProperties = getRestrictedProperties();

    if (!restrictedProperties.isEmpty()) {
      // Attribute names with dashes are converted to camelCase property names by capitalizing the
      // character following each dash, then removing the dashes. For example, the attribute
      // first-name maps to firstName. The same mappings happen in reverse when converting property
      // names to attribute names
      String property = Arrays.stream(attribute.split("-"))
          .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1))
          .collect(Collectors.joining());
      property = property.substring(0, 1).toLowerCase() + property.substring(1);

      if (restrictedProperties.contains(property)) {
        throw new WebforjRestrictedAccessException(
            "The attribute " + attribute + " is restricted and cannot be modified.");
      }
    }

    return setUnrestrictedAttribute(attribute, value);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAttribute(String attribute) {
    // ask the component first
    if (control != null) {
      try {
        return control.getAttribute(attribute);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
    // fall back to the internal list - will not return attributes that are added by
    // default
    return attributes.get(attribute);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeAttribute(String attribute) {
    if (control != null) {
      try {
        control.removeAttribute(attribute);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.attributes.remove(attribute);
    return getSelf();
  }

  /**
   * The getRestrictedAttributes returns a list of attributes that are restricted by the component.
   * The default implementation returns an empty ArrayList, which means that no attributes are
   * restricted. Some components might need to restrict attributes to prevent the API user from
   * setting attributes that are supported by the component and have an already defined behavior.
   *
   * <p>
   * If an attribute is restricted, it also means that the corresponding property version of that
   * attribute is restricted. When converting attribute names to property names, the following
   * process is followed: dashed attribute names are converted to CamelCase property names by
   * removing dashes and capitalizing the next letter of each word. For example, if the attribute
   * name is "first-name", it would be converted to "firstName" as a property.
   * </p>
   *
   * @return A list of restricted attributes, or empty list if no attributes are restricted.
   * @since 23.02
   */
  public List<String> getRestrictedAttributes() {
    List<String> restrictedProperties = getRestrictedProperties();

    if (!restrictedProperties.isEmpty()) {
      return restrictedProperties.stream().map(property -> {
        String[] words = property.split("(?=\\p{Upper})");
        return Arrays.stream(words).map(String::toLowerCase).collect(Collectors.joining("-"));
      }).collect(Collectors.toList());
    }

    return new ArrayList<>();
  }

  void doSetText(String text) {
    String theText = text == null ? "" : text;

    if (control != null) {
      try {
        control.setText(theText);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.text = theText;
  }

  String doGetText() {
    if (control != null) {
      try {
        return control.getText();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return text;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setHtml(String html) {
    doSetText(addHtmlTag(html));
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHtml() {
    return removeHtmlTag(doGetText());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setText(String text) {
    String value = text == null ? "" : text;
    if (!isWrappedWithHtmlTag(value)) {
      value = sanitizeHtml(value);
      doSetText(value);
    } else {
      setHtml(value);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getText() {
    return sanitizeHtml(doGetText());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setStyle(String property, String value) {
    if (control != null) {
      try {
        control.setStyle(property, value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.styles.put(property, value);
    this.removedStyles.remove(property);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    if (control != null) {
      try {
        return control.getStyle(property);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
    // fall back to the internal list - will not return styles that are added by
    // default
    return styles.get(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    if (control != null) {
      try {
        return control.getComputedStyle(property);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }
    // fall back to the internal list - will not return styles that are added by
    // default
    return styles.get(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeStyle(String property) {
    if (control != null) {
      try {
        // Current BBj implementation does not have a remove style method
        control.unsetStyle(property);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.styles.remove(property);
    this.removedStyles.add(property);

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T addClassName(String... classNames) {
    for (String className : classNames) {
      if (control != null) {
        try {
          control.addClass(className);
        } catch (BBjException e) {
          throw new WebforjRuntimeException(e);
        }
      }

      this.classNames.add(className);
      this.removedClassNames.remove(className);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T removeClassName(String... classNames) {
    for (String className : classNames) {
      if (control != null) {
        try {
          control.removeClass(className);
        } catch (BBjException e) {
          throw new WebforjRuntimeException(e);
        }
      }

      this.removedClassNames.add(className);
      this.classNames.remove(className);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setTooltipText(String text) {
    if (control != null) {
      try {
        control.setToolTipText(text);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.tooltipText = text;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getTooltipText() {
    if (control != null) {
      try {
        return control.getToolTipText();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return tooltipText;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setVisible(boolean visible) {
    if (control != null) {
      try {
        control.setVisible(visible);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.visible = visible;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isVisible() {
    if (control != null) {
      try {
        return control.isVisible();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return visible;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setWidth(String width) {
    this.width = width;

    if (width == null) {
      removeStyle("width"); // NOSONAR
    } else {
      setStyle("width", width);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getWidth() {
    return width;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedWidth() {
    return getComputedStyle("width");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMinWidth(String minWidth) {
    this.minWidth = minWidth;

    if (minWidth == null) {
      removeStyle("min-width"); // NOSONAR
    } else {
      setStyle("min-width", minWidth);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMinWidth() {
    return minWidth;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedMinWidth() {
    return getComputedStyle("min-width");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMaxWidth(String maxWidth) {
    this.maxWidth = maxWidth;

    if (maxWidth == null) {
      removeStyle("max-width"); // NOSONAR
    } else {
      setStyle("max-width", maxWidth);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMaxWidth() {
    return maxWidth;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedMaxWidth() {
    return getComputedStyle("max-width");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setHeight(String height) {
    this.height = height;

    if (height == null) {
      removeStyle("height"); // NOSONAR
    } else {
      setStyle("height", height);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getHeight() {
    return height;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedHeight() {
    return getComputedStyle("height");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMinHeight(String minHeight) {
    this.minHeight = minHeight;

    if (minHeight == null) {
      removeStyle("min-height"); // NOSONAR
    } else {
      setStyle("min-height", minHeight);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMinHeight() {
    return minHeight;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedMinHeight() {
    return getComputedStyle("min-height");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMaxHeight(String maxHeight) {
    this.maxHeight = maxHeight;

    if (maxHeight == null) {
      removeStyle("max-height"); // NOSONAR
    } else {
      setStyle("max-height", maxHeight);
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMaxHeight() {
    return maxHeight;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedMaxHeight() {
    return getComputedStyle("max-height");
  }

  /**
   * Adds a MouseEnter event for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<MouseEnterEvent> addMouseEnterListener(
      EventListener<MouseEnterEvent> listener) {
    return mouseEnterEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<MouseEnterEvent> onMouseEnter(
      EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Adds a MouseExit event for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<MouseExitEvent> addMouseExitListener(
      EventListener<MouseExitEvent> listener) {
    return mouseExitEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<MouseExitEvent> onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Adds a RightMouseDown event for the component.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RightMouseDownEvent> addRightMouseDownListener(
      EventListener<RightMouseDownEvent> listener) {
    return rightMouseDownEventSinkListenerRegistry.addEventListener(listener);
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @param listener the event listener to be added
   * @return A registration object for removing the event listener
   */
  public ListenerRegistration<RightMouseDownEvent> onRightMouseDown(
      EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Gets the listeners for the given event class.
   *
   * @param <E> the generic type
   * @param eventClass the event class
   *
   * @return the listeners
   */
  public final <E extends EventObject> List<EventListener<E>> getEventListeners(
      Class<E> eventClass) {
    return dispatcher.getListeners(eventClass);
  }

  /**
   * Get the event dispatcher instance for the component.
   *
   * @return The instance of the event dispatcher.
   */
  protected final EventDispatcher getEventDispatcher() {
    return dispatcher;
  }

  /**
   * Sets the value for a property in the component. This method does not check if the property is
   * restricted or not.
   *
   * @param property the name of the property
   * @param value the value to be set
   *
   * @return The component itself.
   */
  protected T setUnrestrictedProperty(String property, Object value) {
    if (control != null) {
      try {
        control.setProperty(property, value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    properties.put(property, value);

    return getSelf();
  }

  /**
   * Sets the value for a specified component attribute. This method does not check if the attribute
   * is restricted or not.
   *
   * @param attribute the name of the attribute
   * @param value the value to be set
   *
   * @return The component itself.
   */
  protected T setUnrestrictedAttribute(String attribute, String value) {
    if (control != null) {
      try {
        control.setAttribute(attribute, value);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    attributes.put(attribute, value);

    return getSelf();
  }

  /**
   * Sets whether a user can edit the component.
   *
   * @param readOnly true to disable editing, false to enable editing.
   * @return the component itself.
   */
  protected T setComponentReadOnly(boolean readOnly) {
    if (control instanceof Editable) {
      try {
        ((Editable) control).setEditable(!readOnly);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.readOnly = readOnly;
    return getSelf();
  }

  /**
   * Checks whether the component is set to read-only.
   *
   * @return true if the user cannot edit the component, false if editing is allowed.
   */
  protected boolean isComponentReadOnly() {
    if (control instanceof Editable) {
      try {
        return !((Editable) control).isEditable();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this.readOnly;
  }

  /**
   * Set the placeholder of component.
   *
   * @param placeholder the placeholder of the component.
   * @return the component itself.
   */
  protected T setComponentPlaceholder(String placeholder) {
    this.placeholder = placeholder;
    setUnrestrictedProperty("placeholder", placeholder);
    return getSelf();
  }

  /**
   * Get the placeholder of the component.
   *
   * @return the placeholder of component.
   */
  protected String getComponentPlaceholder() {
    return placeholder;
  }

  /**
   * Sets the highlight behavior for the component's text when it receives focus.
   *
   * @param behavior The desired behavior for the component's text when it receives focus.
   * @return The component itself.
   */
  protected T setComponentHighlightOnFocus(HasHighlightOnFocus.Behavior behavior) {
    if (control != null) {
      try {
        if (control instanceof TextControl) {
          ((TextControl) control).setHighlightOnFocus(behavior.getValue());
        }
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.highlightOnFocus = behavior;

    return getSelf();
  }

  /**
   * Gets the highlight behavior for the component's text when it receives focus.
   *
   * @return The highlight behavior for the component's text when it receives focus.
   */
  protected HasHighlightOnFocus.Behavior getComponentHighlightOnFocus() {
    return this.highlightOnFocus;
  }

  /**
   * Sets the component's default horizontal alignment.
   *
   * @param alignment Enum value of alignment
   * @return The component itself.
   */
  protected T setComponentDefaultHorizontalAlignment(HasHorizontalAlignment.Alignment alignment) {
    this.defaultHorizontalAlignment = alignment;
    return getSelf();
  }

  /**
   * Sets the component's horizontal alignment.
   *
   * @param alignment Enum value of alignment
   * @return The component itself.
   */
  protected T setComponentHorizontalAlignment(HasHorizontalAlignment.Alignment alignment) {
    this.horizontalAlignment = alignment;

    if (control instanceof TextAlignable) {
      try {
        ((TextAlignable) control).setAlignment(alignment.getValue());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Gets the components's horizontal alignment.
   *
   * @return the component's horizontal alignment
   */
  protected HasHorizontalAlignment.Alignment getComponentHorizontalAlignment() {
    if (control instanceof TextAlignable) {
      try {
        return HasHorizontalAlignment.Alignment.fromValue(((TextAlignable) control).getAlignment());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    HasHorizontalAlignment.Alignment defaultAlignment =
        this.defaultHorizontalAlignment == null ? HasHorizontalAlignment.Alignment.LEFT
            : this.defaultHorizontalAlignment;

    return this.horizontalAlignment == null ? defaultAlignment : this.horizontalAlignment;
  }

  /**
   * Sets the expanse for the component.
   *
   * @param expanse The component expanse
   * @return The component itself.
   */
  protected <V extends Enum<V> & ExpanseBase> T setComponentExpanse(V expanse) {
    this.expanse = expanse;
    setUnrestrictedProperty("expanse",
        expanse == null || Expanse.NONE.equals(expanse) ? "" : expanse);
    return getSelf();
  }

  /**
   * Gets the expanse of the component.
   *
   * @return The expanse for the component.
   */
  protected <V extends ExpanseBase> V getComponentExpanse() {
    Expanse none = Expanse.NONE;
    if (this.expanse == null) {
      return (V) none;
    }

    @SuppressWarnings("unchecked")
    V theExpanse = (V) this.expanse;
    return theExpanse;
  }

  /**
   * Sets the theme for the component.
   *
   * @param expanse The component theme
   */
  protected <V extends Enum<V> & ThemeBase> void setComponentTheme(V theme) {
    this.theme = theme;
    setUnrestrictedProperty("theme", theme == null ? "" : theme);
  }

  /**
   * Gets the theme of the component.
   *
   * @return The theme for the component.
   */
  protected <V extends ThemeBase> V getComponentTheme() {
    if (this.theme == null) {
      return null;
    }

    @SuppressWarnings("unchecked")
    V theTheme = (V) this.theme;
    return theTheme;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onDestroy() {
    try {
      if (control != null && !control.isDestroyed()) {
        control.destroy();
        control = null;
      }
    } catch (BBjException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  /**
   * Goes through every supported EventSinkListenerRegistry and calls the
   * {@link ComponentEventSinkRegistry#attach()} method.
   *
   * <p>
   * If a the subclass of TypedDwcComponent overrides this method, it must call this method
   * explicitly to ensure that the common event listeners are registered.
   * </p>
   */
  protected void attachControlCallbacks() {
    mouseEnterEventSinkListenerRegistry.attach();
    mouseExitEventSinkListenerRegistry.attach();
    rightMouseDownEventSinkListenerRegistry.attach();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();
    attachControlCallbacks();

    if (!Boolean.TRUE.equals(visible)) {
      this.setVisible(visible);
    }

    if (text != null && !text.isEmpty()) {
      doSetText(text);
    }

    if (readOnly) {
      setComponentReadOnly(readOnly);
    }

    if (!tooltipText.isEmpty()) {
      setTooltipText(tooltipText);
    }

    if (!attributes.isEmpty()) {
      Map<String, String> clone = new HashMap<>(attributes);
      for (Map.Entry<String, String> entry : clone.entrySet()) {
        setUnrestrictedAttribute(entry.getKey(), entry.getValue());
      }
    }

    if (!properties.isEmpty()) {
      Map<String, Object> clone = new HashMap<>(properties);
      for (Map.Entry<String, Object> entry : clone.entrySet()) {
        setUnrestrictedProperty(entry.getKey(), entry.getValue());
      }
    }

    if (!styles.isEmpty()) {
      Map<String, String> clone = new HashMap<>(styles);
      for (Map.Entry<String, String> entry : clone.entrySet()) {
        setStyle(entry.getKey(), entry.getValue());
      }
    }

    if (!removedStyles.isEmpty()) {
      List<String> clone = new ArrayList<>(removedStyles);
      for (String style : clone) {
        removeStyle(style);
      }
    }

    if (!classNames.isEmpty()) {
      List<String> clone = new ArrayList<>(classNames);
      for (String cl : clone) {
        addClassName(cl);
      }
    }

    if (!removedClassNames.isEmpty()) {
      List<String> clone = new ArrayList<>(removedClassNames);
      for (String cl : clone) {
        removeClassName(cl);
      }
    }

    if (highlightOnFocus != HasHighlightOnFocus.Behavior.FOCUS_OR_KEY) {
      setComponentHighlightOnFocus(highlightOnFocus);
    }

    if (horizontalAlignment != null && horizontalAlignment != defaultHorizontalAlignment) {
      setComponentHorizontalAlignment(horizontalAlignment);
    }
  }

  /**
   * Returns an instance of the current class, casted to its generic type. This method is primarily
   * used for method chaining in subclasses of TypedDwcComponent.
   *
   * @return An instance of the current class, casted to its generic type.
   */
  protected final T getSelf() {
    @SuppressWarnings("unchecked")
    T self = (T) this;

    return self;
  }

  private String sanitizeHtml(String html) {
    return (html == null ? "" : html).replaceAll("\\<[^>]*>", "");
  }

  private boolean isWrappedWithHtmlTag(String text) {
    return (text == null ? "" : text).trim().startsWith("<html>");
  }

  private String addHtmlTag(String text) {
    String value = text == null ? "" : text;
    if (!isWrappedWithHtmlTag(value)) {
      value = "<html>" + value + "</html>";
    }

    return value;
  }

  private String removeHtmlTag(String text) {
    String value = text == null ? "" : text;

    if (value.startsWith("<html>")) {
      value = value.substring(6);
    }

    if (value.endsWith("</html>")) {
      value = value.substring(0, value.length() - 7);
    }

    return value;
  }
}
