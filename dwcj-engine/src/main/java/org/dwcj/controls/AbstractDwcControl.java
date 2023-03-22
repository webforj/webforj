package org.dwcj.controls;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.dwcj.Environment;
import org.dwcj.interfaces.HasAttribute;
import org.dwcj.interfaces.HasText;
import org.dwcj.interfaces.HasClassName;
import org.dwcj.interfaces.HasStyle;
import org.dwcj.interfaces.HasEnable;
import org.dwcj.interfaces.HasMouseWheelCondition;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.interfaces.TextHighlightable;
import org.dwcj.interfaces.HasTooltip;
import org.dwcj.interfaces.HasVisibility;

/**
 * The base class for most DWC/BBj controls. Extends the AbstractControl class,
 * and implements
 * default behaviors for the implemented interface methods.
 */
public abstract class AbstractDwcControl extends AbstractControl
    implements HasAttribute, HasText, HasClassName, HasStyle, HasEnable, HasTooltip, HasVisibility {

  /*
   * =============================================================================
   * Members implemented for interfacing with BBj methods/implementations
   * =============================================================================
   */
  public static final String STR_EXPANSE = "expanse";
  public static final String STR_THEME = "theme";
  protected static final BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
  protected static final BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
  protected static final BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

  /*
   * =============================================================================
   * Members common to all inheriting controls
   * =============================================================================
   */
  private String text = "";
  private Boolean visible = true;
  private Boolean enabled = true;
  private String tooltipText = "";

  private final Map<String, String> addAttributes = new HashMap<>();
  private final List<String> addCssClasses = new ArrayList<>();
  private final Map<String, String> addStyles = new HashMap<>();

  private final List<String> removeAttributes = new ArrayList<>();
  private final List<String> removeStyles = new ArrayList<>();
  private final List<String> removeCssClasses = new ArrayList<>();

  /*
   * =============================================================================
   * Theme and Expanse variables which need to be enumerated in their respective
   * child components
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
  protected Boolean focusable = null;
  protected Boolean tabTraversable = null;
  protected TextAlignable.Alignment textAlignment = null;
  protected Integer horizontalScrollBarPosition = null;
  protected Integer verticalScrollBarPosition = null;
  protected HasMouseWheelCondition.MouseWheelCondition mouseWheelCondition = null;
  protected TextHighlightable.Highlight textHighlight = null;

  /**
   * Gets the value for an attribute in the control
   * 
   * @param attribute the name of the attribute
   * @return the attribute
   */
  @Override
  public String getAttribute(String attribute) {
    // ask the control first
    if (ctrl != null) {
      try {
        return ctrl.getAttribute(attribute);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    // fall back to the internal list - will not return attributes that are added by
    // default
    return addAttributes.get(attribute);
  }

  /**
   * Set the value for an attribute in the
   * 
   * @param attribute the name of the attribute
   * @param value     the value to be set
   * @return the control itself
   */
  @Override
  public AbstractDwcControl setAttribute(String attribute, String value) {
    if (ctrl != null) {
      try {
        ctrl.setAttribute(attribute, value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      addAttributes.put(attribute, value);
      removeAttributes.remove(attribute);
    }
    return this;
  }

  /**
   * Removes an attribute from the control
   * 
   * @param attribute the name of the attribute
   * @return the control itself
   */
  @Override
  public AbstractDwcControl removeAttribute(String attribute) {
    if (ctrl != null) {
      try {
        ctrl.removeAttribute(attribute);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      removeAttributes.add(attribute);
      addAttributes.remove(attribute);
    }
    return this;
  }

  /**
   * Gets the text of the control
   * 
   * @return Text of the control
   */
  @Override
  public String getText() {
    if (ctrl != null)
      try {
        return ctrl.getText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    return text;
  }

  /**
   * Sets the text of the control
   * 
   * @param text The desired text of the control
   * @return the control itself
   */
  @Override
  public AbstractDwcControl setText(String text) {
    if (ctrl != null) {
      try {
        ctrl.setText(text);
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
   * Gets the style value for the input property for the control
   * 
   * @param property The desired property
   * @return Style value of given property
   */
  @Override
  public String getStyle(String property) {
    if (ctrl != null) {
      try {
        return ctrl.getComputedStyle(property);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    // fall back to the internal list - will not return styles that are added by
    // default
    return addStyles.get(property);
  }

  /**
   * Sets the style value for the input property for the control
   * 
   * @param property The desired style property
   * @param property The desired value for given property
   * @return The control itself
   */
  @Override
  public AbstractDwcControl setStyle(String property, String value) {
    if (ctrl != null) {
      try {
        ctrl.setStyle(property, value);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.addStyles.put(property, value);
      this.removeStyles.remove(property);
    }
    return this;
  }

  /**
   * Removes the style value for the input property for the control
   * 
   * @param property The desired style property to be removed
   * @return The control itself
   */
  @Override
  public AbstractDwcControl removeStyle(String property) {
    if (ctrl != null) {
      try {
        // Current BBj implementation does not have a remove style method
        ctrl.unsetStyle(property);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.addStyles.remove(property);
      this.removeStyles.add(property);
    }
    return this;
  }

  /**
   * Adds a class attribute to the control
   * 
   * @param property The desired class
   * @return The control itself
   */
  @Override
  public AbstractDwcControl addClassName(String selector) {
    if (ctrl != null) {
      try {
        ctrl.addClass(selector);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.addCssClasses.add(selector);
      this.removeCssClasses.remove(selector);
    }
    return this;
  }

  /**
   * Removes a class attribute from the control
   * 
   * @param property The desired class
   * @return The control itself
   */
  @Override
  public AbstractDwcControl removeClassName(String selector) {
    if (ctrl != null) {
      try {
        ctrl.removeClass(selector);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.removeCssClasses.add(selector);
      this.addCssClasses.remove(selector);
    }
    return this;
  }

  /**
   * Returns whether or not the control is enabled
   * 
   * @return True if control is enabled, false otherwise
   */
  @Override
  public Boolean isEnabled() {
    if (this.ctrl != null)
      try {
        return ctrl.isEnabled();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    return enabled;
  }

  /**
   * Sets whether or not the control is enabled
   * 
   * @param enabled Desired boolean for enabled status of control
   * @return The control itself
   */
  @Override
  public AbstractDwcControl setEnabled(Boolean enabled) {
    if (this.ctrl != null)
      try {
        ctrl.setEnabled(enabled);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    this.enabled = enabled;
    return this;
  }

  /**
   * Gets the tooltip text for a control
   * 
   * @return A string with the tooltip text for the control
   */
  @Override
  public String getTooltipText() {
    if (this.ctrl != null)
      try {
        return ctrl.getToolTipText();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    return tooltipText;
  }

  /**
   * Sets the tooltip text for a control
   * 
   * @param text A string with the tooltip text for the control
   * @return The control itself
   */
  @Override
  public AbstractDwcControl setTooltipText(String text) {
    if (this.ctrl != null)
      try {
        ctrl.setToolTipText(text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    this.tooltipText = text;
    return this;
  }

  /**
   * Gets whether or not the control is visible
   * 
   * @return The visibility of the control
   */
  @Override
  public Boolean isVisible() {
    if (this.ctrl != null)
      try {
        return ctrl.isVisible();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    return visible;
  }

  /**
   * Sets whether or not the control is visible
   * 
   * @param Boolean for desired visibility of the control
   * @return The control itself
   */
  @Override
  public AbstractDwcControl setVisible(Boolean visible) {
    if (this.ctrl != null)
      try {
        ctrl.setVisible(visible);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    this.visible = visible;
    return this;
  }

  /**
   * Implementation to allow child controls to utilize base class Theme setters
   * with their own option-appropriate Enums.
   * 
   * @param theme Control-specific theme value
   */

  protected void setControlTheme(Enum<?> theme) {
    if (ctrl != null)
      try {
        switch (theme.toString()) {
          case "DEFAULT":
            ctrl.setAttribute(STR_THEME, "default");
            break;
          case "DANGER":
            ctrl.setAttribute(STR_THEME, "danger");
            break;
          case "GRAY":
            ctrl.setAttribute(STR_THEME, "gray");
            break;
          case "INFO":
            ctrl.setAttribute(STR_THEME, "info");
            break;
          case "PRIMARY":
            ctrl.setAttribute(STR_THEME, "primary");
            break;
          case "SUCCESS":
            ctrl.setAttribute(STR_THEME, "success");
            break;
          case "WARNING":
            ctrl.setAttribute(STR_THEME, "warning");
            break;
          case "OUTLINED_DANGER":
            ctrl.setAttribute(STR_THEME, "outlined-danger");
            break;
          case "OUTLINED_DEFAULT":
            ctrl.setAttribute(STR_THEME, "outlined-default");
            break;
          case "OUTLINED_GRAY":
            ctrl.setAttribute(STR_THEME, "outlined-gray");
            break;
          case "OUTLINED_INFO":
            ctrl.setAttribute(STR_THEME, "outlined-info");
            break;
          case "OUTLINED_SUCCESS":
            ctrl.setAttribute(STR_THEME, "outlined-success");
            break;
          case "OUTLINED_WARNING":
            ctrl.setAttribute(STR_THEME, "outlined-warning");
            break;
          case "OUTLINED_PRIMARY":
            ctrl.setAttribute(STR_THEME, "outlined-primary");
            break;
          default:
            // noop
        }
      } catch (BBjException e) {
        Environment.logError(e);
      }
    this.theme = theme;
  }

  /**
   * Implementation to allow child controls to utilize base class Expanse setters
   * with their own option-appropriate Enums.
   * 
   * @param theme Control-specific theme value
   */
  protected void setControlExpanse(Enum<?> expanse) {
    if (ctrl != null)
      try {
        switch (expanse.toString()) {
          case "LARGE":
            ctrl.setAttribute(STR_EXPANSE, "l");
            break;
          case "MEDIUM":
            ctrl.setAttribute(STR_EXPANSE, "m");
            break;
          case "SMALL":
            ctrl.setAttribute(STR_EXPANSE, "s");
            break;
          case "XLARGE":
            ctrl.setAttribute(STR_EXPANSE, "xl");
            break;
          case "XSMALL":
            ctrl.setAttribute(STR_EXPANSE, "xs");
            break;
          case "XXSMALL":
            ctrl.setAttribute(STR_EXPANSE, "xxs");
            break;
          case "XXXSMALL":
            ctrl.setAttribute(STR_EXPANSE, "xxxs");
            break;
          default:
            // noop
        }
      } catch (BBjException e) {
        Environment.logError(e);
      }
    this.expanse = expanse;
  }

  /**
   * The catchUp method is used to replay attributes and settings that the API
   * user might have
   * added to a control before its creation. A control is not created before it's
   * added
   * to a panel. Anything that is added between instantiation of a control and its
   * addition to a panel
   * has to be recorded and replayed in this method
   *
   * @throws IllegalAccessException - thrown if an attempt is made to call this
   *                                method more than once
   */
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
  @Override
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");
    super.catchUp();

    if (!this.text.isEmpty()) {
      try {
        ctrl.setText(this.text);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    if (!Boolean.TRUE.equals(this.visible)) {
      this.setVisible(this.visible);
    }

    if (!Boolean.TRUE.equals(this.enabled)) {
      this.setEnabled(this.enabled);
    }

    if (!this.tooltipText.isEmpty()) {
      try {
        ctrl.setToolTipText(this.tooltipText);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }

    if (!this.addAttributes.isEmpty()) {
      for (Map.Entry<String, String> entry : this.addAttributes.entrySet()) {
        this.setAttribute(entry.getKey(), entry.getValue());
      }
    }

    if (!this.removeAttributes.isEmpty()) {
      for (String attribute : this.removeAttributes) {
        this.removeAttribute(attribute);
      }
    }

    if (!this.addStyles.isEmpty()) {
      for (Map.Entry<String, String> entry : this.addStyles.entrySet()) {
        this.setStyle(entry.getKey(), entry.getValue());
      }
    }

    if (!this.removeStyles.isEmpty()) {
      for (String style : this.removeStyles) {
        this.removeStyle(style);
      }
    }

    if (!this.addCssClasses.isEmpty()) {
      for (String cl : this.addCssClasses) {
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

  }

  /**
   * Method to destroy a control
   */
  @Override
  public void destroy() {
    this.destroyed = true;
    try {
      if (ctrl != null && !ctrl.isDestroyed()) {
        ctrl.destroy();
      }
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }

}
