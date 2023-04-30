package org.dwcj.component;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcjRuntimeException;
import com.basis.bbj.proxies.sysgui.Focusable;

/**
 * The base class for most DWC/BBj components. Extends the AbstractComponent class, and implements
 * default behaviors for the implemented interface methods.
 */
public abstract class AbstractDwcComponent extends AbstractComponent implements HasAttribute,
    HasText, HasClassName, HasStyle, HasTooltip, HasVisibility, HasProperty {

  /*
   * ============================================================================= Members
   * implemented for interfacing with BBj methods/implementations
   * =============================================================================
   */
  public static final String STR_EXPANSE = "expanse";
  public static final String STR_THEME = "theme";
  protected static final BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
  protected static final BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
  protected static final BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

  /*
   * ============================================================================= Members common to
   * all inheriting components
   * =============================================================================
   */
  private String text = "";
  private Boolean visible = true;
  private Boolean enabled = true;
  private String tooltipText = "";

  private final Map<String, String> styles = new HashMap<>();
  private final List<String> removeStyles = new ArrayList<>();

  private final List<String> cssClasses = new ArrayList<>();
  private final List<String> removeCssClasses = new ArrayList<>();

  private final Map<String, String> attributes = new HashMap<>();
  private final List<String> removeAttributes = new ArrayList<>();

  private final Map<String, Object> properties = new HashMap<>();

  /*
   * ============================================================================= Theme and Expanse
   * variables which need to be enumerated in their respective child components
   * =============================================================================
   */
  private Enum<?> theme = null;
  private Enum<?> expanse = null;

  /*
   * =============================================================================
   * Interface-controlled members
   * =============================================================================
   */
  protected Boolean readOnly = null;
  protected Boolean wasFocused = null;
  protected Boolean tabTraversable = null;
  protected TextAlignable.Alignment textAlignment = null;
  protected Integer horizontalScrollBarPosition = null;
  protected Integer verticalScrollBarPosition = null;
  protected HasMouseWheelCondition.MouseWheelCondition mouseWheelCondition = null;
  protected TextHighlightable.Highlight textHighlight = null;

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
        Environment.logError(e);
      }
    }
    // fall back to the internal list - will not return attributes that are added by
    // default
    return attributes.get(attribute);
  }

  /**
   * Set the value for a specified component attribute.
   *
   * @param attribute the name of the attribute
   * @param value the value to be set
   * @return the component itself
   */
  @Override
  public AbstractDwcComponent setAttribute(String attribute, String value) {
    if (control != null) {
      try {
        control.setAttribute(attribute, value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      attributes.put(attribute, value);
      removeAttributes.remove(attribute);
    }
    return this;
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
        Environment.logError(e);
      }
    } else {
      removeAttributes.add(attribute);
      attributes.remove(attribute);
    }
    return this;
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
        Environment.logError(e);
      }
    }
    return properties.get(property);
  }

  /**
   * Set the value for a property in the component.
   *
   * @param property the name of the property
   * @param value the value to be set
   * @return the component itself
   */
  @Override
  public AbstractDwcComponent setProperty(String property, Object value) {
    if (control != null) {
      try {
        control.putClientProperty(property, value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      properties.put(property, value);
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
   * Implementation to allow child components to utilize base class Expanse setters with their own
   * option-appropriate Enums.
   *
   * @param theme Component-specific theme value
   */
  protected void setControlExpanse(Enum<?> expanse) {
    if (control != null) {
      try {
        switch (expanse.toString()) {
          case "LARGE":
            control.setAttribute(STR_EXPANSE, "l");
            break;
          case "MEDIUM":
            control.setAttribute(STR_EXPANSE, "m");
            break;
          case "SMALL":
            control.setAttribute(STR_EXPANSE, "s");
            break;
          case "XLARGE":
            control.setAttribute(STR_EXPANSE, "xl");
            break;
          case "XSMALL":
            control.setAttribute(STR_EXPANSE, "xs");
            break;
          case "XXSMALL":
            control.setAttribute(STR_EXPANSE, "xxs");
            break;
          case "XXXSMALL":
            control.setAttribute(STR_EXPANSE, "xxxs");
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
   * The catchUp method is used to replay attributes and settings that the API user might have added
   * to a component before its creation. A component is not created before it's added to a panel.
   * Anything that is added between instantiation of a component and its addition to a panel has to
   * be recorded and replayed in this method
   *
   * @throws IllegalAccessException - thrown if an attempt is made to call this method more than
   *         once
   */
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
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

    if (!this.tooltipText.isEmpty()) {
      try {
        control.setToolTipText(this.tooltipText);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    if (!this.attributes.isEmpty()) {
      for (Map.Entry<String, String> entry : this.attributes.entrySet()) {
        this.setAttribute(entry.getKey(), entry.getValue());
      }
    }

    if (!this.removeAttributes.isEmpty()) {
      for (String attribute : this.removeAttributes) {
        this.removeAttribute(attribute);
      }
    }

    if (!this.properties.isEmpty()) {
      for (Map.Entry<String, Object> entry : this.properties.entrySet()) {
        this.setProperty(entry.getKey(), entry.getValue());
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

}
