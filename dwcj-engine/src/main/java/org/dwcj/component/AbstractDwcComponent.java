package org.dwcj.component;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.Editable;
import com.basis.bbj.proxies.sysgui.Focusable;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcjRestrictedAccessException;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The base class for most DWC/BBj components. Extends the AbstractComponent class, and implements
 * default behaviors for the implemented interface methods.
 */
public abstract class AbstractDwcComponent extends AbstractComponent implements HasAttribute,
    HasText, HasClassName, HasStyle, HasTooltip, HasVisibility, HasProperty {
  public static final String STR_EXPANSE = "expanse";
  public static final String STR_THEME = "theme";

  protected static final BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
  protected static final BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
  protected static final BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);
  protected boolean readOnly = false;
  protected Boolean wasFocused = null;
  protected Boolean tabTraversable = null;
  protected HorizontalAlignment.Alignment textAlignment = null;
  protected Integer horizontalScrollBarPosition = null;
  protected Integer verticalScrollBarPosition = null;
  protected HasMouseWheelCondition.MouseWheelCondition mouseWheelCondition = null;
  protected TextHighlightable.Highlight textHighlight = null;
  protected BBjControl control;

  private String text = "";
  private Boolean visible = true;
  private Boolean enabled = true;
  private String tooltipText = "";
  private final Map<String, String> styles = new HashMap<>();
  private final List<String> removeStyles = new ArrayList<>();
  private final List<String> cssClasses = new ArrayList<>();
  private final List<String> removeCssClasses = new ArrayList<>();
  private Enum<?> theme = null;
  private Enum<?> expanse = null;
  private final Map<String, String> attributes = new ConcurrentHashMap<>();
  private final Map<String, Object> properties = new ConcurrentHashMap<>();
  private Enum<? extends ExpanseBase> componentExpanse = null;

  /**
   * Set the value for a property in the component.
   *
   * @param property the name of the property
   * @param value the value to be set
   * @return the component itself
   *
   * @throws DwcjRestrictedAccessException if the property is restricted
   */
  @Override
  public AbstractDwcComponent setProperty(String property, Object value) {
    List<String> restrictedProperties = getRestrictedProperties();
    if (!restrictedProperties.isEmpty() && restrictedProperties.contains(property)) {
      throw new DwcjRestrictedAccessException(
          "The property '" + property + "' is restricted and cannot be modified.");
    }

    return setUnrestrictedProperty(property, value);
  }

  /**
   * Gets the value for a property in the component.
   *
   * @param property the name of the property
   * @return the value of the property
   */
  @Override
  public Object getProperty(String property) {
    if (control != null) {
      try {
        return control.getClientProperty(property);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return properties.get(property);
  }

  /**
   * Gets all properties of the component.
   *
   * @return a map of all properties
   * @since 23.02
   */
  public Map<String, Object> getProperties() {
    return Collections.unmodifiableMap(properties);
  }

  /**
   * The getRestrictedProperties returns a list of properties that are restricted by the component.
   * The default implementation returns an empty ArrayList, which means that no properties are
   * restricted. Some components might need to restrict properties to prevent the API user from
   * setting properties that are supported by the component and has a already defined behavior.
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
   * Set the value for a specified component attribute.
   *
   * @param attribute the name of the attribute
   * @param value the value to be set
   * @return the component itself
   *
   * @throws DwcjRestrictedAccessException if the attribute is restricted
   */
  @Override
  public AbstractDwcComponent setAttribute(String attribute, String value) {
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
        throw new DwcjRestrictedAccessException(
            "The attribute " + attribute + " is restricted and cannot be modified.");
      }
    }

    return setUnrestrictedAttribute(attribute, value);
  }

  /**
   * Gets the value for a specific attribute in the component.
   *
   * @param attribute the name of the attribute
   * @return the attribute
   */
  @Override
  public String getAttribute(String attribute) {
    // ask the component first
    if (control != null) {
      try {
        return control.getAttribute(attribute);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    // fall back to the internal list - will not return attributes that are added by
    // default
    return attributes.get(attribute);
  }

  /**
   * Removes an attribute from the component.
   *
   * @param attribute the name of the attribute
   * @return the component itself
   */
  @Override
  public AbstractDwcComponent removeAttribute(String attribute) {
    if (control != null) {
      try {
        control.removeAttribute(attribute);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    attributes.remove(attribute);
    return this;
  }

  /**
   * Gets all attributes of the component.
   *
   * @return a map of all attributes
   * @since 23.02
   */
  public Map<String, String> getAttributes() {
    return Collections.unmodifiableMap(attributes);
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

  /**
   * Sets the text of the component.
   *
   * @param text The desired text of the component
   * @return the component itself
   */
  @Override
  public AbstractDwcComponent setText(String text) {
    if (control != null) {
      try {
        control.setText(text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    if (text != null) {
      this.text = new String(text.getBytes());
    } else {
      this.text = "<null>";
    }
    return this;
  }

  /**
   * Gets the text of the component.
   *
   * @return Text of the component
   */
  @Override
  public String getText() {
    if (control != null) {
      try {
        return control.getText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return text;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public AbstractDwcComponent setStyle(String property, String value) {
    if (control != null) {
      try {
        control.setStyle(property, value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.styles.put(property, value);
      this.removeStyles.remove(property);
    }
    return this;
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
        Environment.logError(e);
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
        Environment.logError(e);
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
  public AbstractDwcComponent removeStyle(String property) {
    if (control != null) {
      try {
        // Current BBj implementation does not have a remove style method
        control.unsetStyle(property);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.styles.remove(property);
      this.removeStyles.add(property);
    }
    return this;
  }

  /**
   * Adds a class attribute to the component.
   *
   * @param selector The desired class
   * @return The component itself
   */
  @Override
  public AbstractDwcComponent addClassName(String selector) {
    if (control != null) {
      try {
        control.addClass(selector);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.cssClasses.add(selector);
      this.removeCssClasses.remove(selector);
    }
    return this;
  }

  /**
   * Removes a class attribute from the component.
   *
   * @param selector The desired class
   * @return The component itself
   */
  @Override
  public AbstractDwcComponent removeClassName(String selector) {
    if (control != null) {
      try {
        control.removeClass(selector);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.removeCssClasses.add(selector);
      this.cssClasses.remove(selector);
    }
    return this;
  }

  /**
   * Sets the tooltip text for a component.
   *
   * @param text A string with the tooltip text for the component
   * @return The component itself
   */
  @Override
  public AbstractDwcComponent setTooltipText(String text) {
    if (this.control != null) {
      try {
        control.setToolTipText(text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.tooltipText = text;
    return this;
  }

  /**
   * Gets the tooltip text for a component.
   *
   * @return A string with the tooltip text for the component
   */
  @Override
  public String getTooltipText() {
    if (this.control != null) {
      try {
        return control.getToolTipText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return tooltipText;
  }

  /**
   * Sets whether or not the is component is visible on the page, true if so false if not.
   *
   * @param visible for desired visibility of the component
   *
   * @return The component itself
   */
  @Override
  public AbstractDwcComponent setVisible(Boolean visible) {
    if (this.control != null) {
      try {
        control.setVisible(visible);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.visible = visible;
    return this;
  }

  /**
   * Gets whether or not the component is visible.
   *
   * @return The visibility of the component
   */
  @Override
  public Boolean isVisible() {
    if (this.control != null) {
      try {
        return control.isVisible();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return visible;
  }

  /**
   * Method to destroy a component.
   */
  @Override
  public void destroy() {
    this.destroyed = true;
    try {
      if (control != null && !control.isDestroyed()) {
        control.destroy();
      }
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

  /**
   * This method gets the underlying original BBj control It's package private and can only be
   * accessed through the ControlAccessor No API user / customer shall ever work directly with BBj
   * controls.
   *
   * @return the underlying BBj control
   */
  BBjControl getControl() {
    return this.control;
  }

  /**
   * This method sets the underlying original BBj control. It's package private and can only be
   * accessed through the ControlAccessor No API user / customer shall ever work directly with BBj
   * controls.
   *
   * @param control the BBj control to set.
   */
  protected void setControl(BBjControl control) {
    this.control = control;
  }

  /**
   * Set the value for a property in the component. This method does not check if the property is
   * restricted or not.
   *
   * @param property the name of the property
   * @param value the value to be set
   *
   * @return the component itself
   */
  protected AbstractDwcComponent setUnrestrictedProperty(String property, Object value) {
    if (control != null) {
      try {
        control.putClientProperty(property, value);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    properties.put(property, value);
    return this;
  }

  /**
   * Set the value for a specified component attribute. This method does not check if the attribute
   * is restricted or not.
   *
   * @param attribute the name of the attribute
   * @param value the value to be set
   *
   * @return the component itself
   */
  protected AbstractDwcComponent setUnrestrictedAttribute(String attribute, String value) {
    if (control != null) {
      try {
        control.setAttribute(attribute, value);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    attributes.put(attribute, value);
    return this;
  }

  /**
   * Sets whether or not the component is enabled.
   *
   * @param enabled Desired boolean for enabled status of component
   * @return The component itself
   */
  protected AbstractDwcComponent setComponentEnabled(boolean enabled) {
    if (this.control != null) {
      try {
        control.setEnabled(enabled);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.enabled = enabled;
    return this;
  }

  /**
   * Returns whether or not the component is enabled.
   *
   * @return True if component is enabled, false otherwise
   */
  protected Boolean isComponentEnabled() {
    if (this.control != null) {
      try {
        return control.isEnabled();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return enabled;
  }

  /**
   * Sets whether or not the component is read only.
   *
   * @param readOnly Desired boolean for read only status of component
   * @return The component itself
   */
  protected AbstractDwcComponent setComponentReadOnly(boolean readOnly) {
    if (this.control instanceof Editable) {
      try {
        ((Editable) control).setEditable(readOnly);
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    this.readOnly = readOnly;
    return this;
  }

  /**
   * Returns whether or not the component is read only.
   *
   * @return True if component is read only, false otherwise
   */
  protected Boolean isComponentReadOnly() {
    if (this.control instanceof Editable) {
      try {
        return ((Editable) control).isEditable();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }

    return this.readOnly;
  }

  /**
   * Gives a component focus when it is added to the window. Note that if this method is called on
   * multiple components, focus will be given to the component added latest to the window.
   *
   * @return The component itself
   */
  protected AbstractDwcComponent focusComponent() throws DwcjRuntimeException {
    if (this.control != null) {
      try {
        control.focus();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.wasFocused = true;
    return this;
  }

  /**
   * Sets whether or not the component can gain focus via navigation of the page using the Tab key.
   *
   * @param Boolean value for desired tab traversability status.
   * @return The component itself.
   */
  protected AbstractDwcComponent setComponentTabTraversable(boolean value)
      throws DwcjRuntimeException {
    if (this.control != null) {
      try {
        if (control instanceof Focusable) {
          ((Focusable) control).setFocusable(value);
        }
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
      }
    }
    this.tabTraversable = value;
    return this;
  }

  /*
   * Returns whether or not a component can receive focus via navigation with the Tab key.
   *
   * @return True is component can be focused, false if not.
   */
  protected Boolean isComponentTabTraversable() {
    return this.tabTraversable;
  }

  /**
   * Sets the expanse for the component.
   *
   * @param expanse The component expanse
   * @since 23.02
   */
  protected <V extends Enum<V> & ExpanseBase> void setComponentExpanse(V expanse) {
    this.componentExpanse = expanse;
    setUnrestrictedProperty("expanse", expanse.getValue());
  }

  /**
   * Get the expanse of the component.
   *
   * @return The expanse for the component.
   * @since 23.02
   */
  protected Enum<? extends ExpanseBase> getComponentExpanse() {
    return this.componentExpanse;
  }

  /**
   * Implementation to allow child components to utilize base class Expanse setters with their own
   * option-appropriate Enums.
   *
   * @param theme Component-specific theme value
   * @deprecated The method is deprecated since v23.02 and will be removed in future versions. Use
   *             {@link #setComponentExpanse(Enum)} instead.
   */
  @Deprecated
  protected void setControlExpanse(Enum<?> expanse) {
    if (control != null) {
      try {
        switch (expanse.toString()) {
          case "LARGE":
            control.putClientProperty(STR_EXPANSE, "l");
            break;
          case "MEDIUM":
            control.putClientProperty(STR_EXPANSE, "m");
            break;
          case "SMALL":
            control.putClientProperty(STR_EXPANSE, "s");
            break;
          case "XLARGE":
            control.putClientProperty(STR_EXPANSE, "xl");
            break;
          case "XSMALL":
            control.putClientProperty(STR_EXPANSE, "xs");
            break;
          case "XXSMALL":
            control.putClientProperty(STR_EXPANSE, "xxs");
            break;
          case "XXXSMALL":
            control.putClientProperty(STR_EXPANSE, "xxxs");
            break;
          default:
            // noop
        }
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.expanse = expanse;
  }

  /**
   * Implementation to allow child components to utilize base class Theme setters with their own
   * option-appropriate Enums.
   *
   * @param theme Component-specific theme value
   */

  protected void setControlTheme(Enum<?> theme) {
    if (control != null) {
      try {
        switch (theme.toString()) {
          case "DEFAULT":
            control.setAttribute(STR_THEME, "default");
            break;
          case "DANGER":
            control.setAttribute(STR_THEME, "danger");
            break;
          case "GRAY":
            control.setAttribute(STR_THEME, "gray");
            break;
          case "INFO":
            control.setAttribute(STR_THEME, "info");
            break;
          case "PRIMARY":
            control.setAttribute(STR_THEME, "primary");
            break;
          case "SUCCESS":
            control.setAttribute(STR_THEME, "success");
            break;
          case "WARNING":
            control.setAttribute(STR_THEME, "warning");
            break;
          case "OUTLINED_DANGER":
            control.setAttribute(STR_THEME, "outlined-danger");
            break;
          case "OUTLINED_DEFAULT":
            control.setAttribute(STR_THEME, "outlined-default");
            break;
          case "OUTLINED_GRAY":
            control.setAttribute(STR_THEME, "outlined-gray");
            break;
          case "OUTLINED_INFO":
            control.setAttribute(STR_THEME, "outlined-info");
            break;
          case "OUTLINED_SUCCESS":
            control.setAttribute(STR_THEME, "outlined-success");
            break;
          case "OUTLINED_WARNING":
            control.setAttribute(STR_THEME, "outlined-warning");
            break;
          case "OUTLINED_PRIMARY":
            control.setAttribute(STR_THEME, "outlined-primary");
            break;
          default:
            // noop
        }
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.theme = theme;
  }

  /**
   * The catchUp method is used to replay attributes and settings that the API user might have added
   * to a component before its creation. A component is not created before it's added to a panel.
   * Anything that is added between instantiation of a component and its addition to a panel has to
   * be recorded and replayed in this method
   *
   * @throws IllegalAccessException - thrown if an attempt is made to call this method more than
   *         once
   */
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (!this.text.isEmpty()) {
      try {
        control.setText(this.text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    if (!Boolean.TRUE.equals(this.visible)) {
      this.setVisible(this.visible);
    }

    if (!Boolean.TRUE.equals(this.enabled)) {
      this.setComponentEnabled(this.enabled);
    }

    if (this.readOnly) {
      this.setComponentReadOnly(this.readOnly);
    }

    if (!this.tooltipText.isEmpty()) {
      try {
        control.setToolTipText(this.tooltipText);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    if (!this.attributes.isEmpty()) {
      for (Map.Entry<String, String> entry : this.attributes.entrySet()) {
        setUnrestrictedAttribute(entry.getKey(), entry.getValue());
      }
    }

    if (!this.properties.isEmpty()) {
      for (Map.Entry<String, Object> entry : this.properties.entrySet()) {
        setUnrestrictedProperty(entry.getKey(), entry.getValue());
      }
    }

    if (!this.styles.isEmpty()) {
      for (Map.Entry<String, String> entry : this.styles.entrySet()) {
        this.setStyle(entry.getKey(), entry.getValue());
      }
    }

    if (!this.removeStyles.isEmpty()) {
      for (String style : this.removeStyles) {
        this.removeStyle(style);
      }
    }

    if (!this.cssClasses.isEmpty()) {
      for (String cl : this.cssClasses) {
        this.addClassName(cl);
      }
    }

    if (!this.removeCssClasses.isEmpty()) {
      for (String cl : this.removeCssClasses) {
        this.removeClassName(cl);
      }
    }

    if (!this.userData.isEmpty()) {
      for (Map.Entry<String, Object> cl : this.userData.entrySet()) {
        this.setUserData(cl.getKey(), cl.getValue());
      }
    }

    if (this.theme != null) {
      this.setControlTheme(this.theme);
    }

    if (this.expanse != null) {
      this.setControlExpanse(this.expanse);
    }

    if (this.tabTraversable != null) {
      this.setComponentTabTraversable(this.tabTraversable);
    }

    if (this.wasFocused != null) {
      this.focusComponent();
    }
  }
}
