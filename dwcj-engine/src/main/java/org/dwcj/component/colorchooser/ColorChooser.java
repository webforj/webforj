package org.dwcj.component.colorchooser;

import com.basis.bbj.proxies.sysgui.BBjColorChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.sysgui.BBjColor;
import java.awt.Color;
import org.dwcj.Environment;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.colorchooser.event.ColorChooserApproveEvent;
import org.dwcj.component.colorchooser.event.ColorChooserCancelEvent;
import org.dwcj.component.colorchooser.event.ColorChooserChangeEvent;
import org.dwcj.component.event.BlurEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.component.event.RightMouseDownEvent;
import org.dwcj.component.event.sink.BlurEventSink;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

/**
 * The class itself extending the abstract DWC Component and implementing interfaces.
 */
public final class ColorChooser extends AbstractDwcComponent
  implements HasEnable, HasFocus, TabTraversable {
  private boolean areButtonsShown = true;
  private String approveText = "OK";
  private String cancelText = "Cancel";
  private boolean isPreviewPanelVisible = true;
  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private FocusEventSink focusEventSink;
  private BlurEventSink blurEventSink;

  /**
   * Enum values with respective values for the expanse of colorChooser.
   */
  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL, XXSMALL, XXXSMALL;
  }

  /**
   * Enum values with respective values for the theme of colorChooser.
   */
  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING;
  }

  public ColorChooser() {
    this.tabTraversable = true;
  }

  public ColorChooser(Color color) {
    this.setColor(color);
    this.tabTraversable = true;
  }

  @Override
  protected void create(AbstractWindow p) {

    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      byte[] flags =
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      this.setControl(w.addColorChooser(new BBjColor(getColor()), flags));
      this.mouseEnterEventSink = new MouseEnterEventSink(this, dispatcher);
      this.mouseExitEventSink = new MouseExitEventSink(this, dispatcher);
      this.rightMouseDownEventSink = new RightMouseDownEventSink(this, dispatcher);
      this.focusEventSink = new FocusEventSink(this, dispatcher);
      this.blurEventSink = new BlurEventSink(this, dispatcher);
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Adds an approve event for the colorChooser component.
   *
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser addColorChooserApproveListener(EventListener<ColorChooserApproveEvent>
                                                       listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(ColorChooserApproveEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(ColorChooserApproveEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addColorChooserApproveListener method.
   *
   * @see ColorChooser#addColorChooserApproveListener(EventListener)
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser onApprove(EventListener<ColorChooserApproveEvent> listener) {
    return addColorChooserApproveListener(listener);
  }

  /**
   * Removes an approve event from the colorchooser component.
   *
   * @param listener the event listener to be removed
   * @return The colorChooser itself
   */
  public ColorChooser removeColorChooserApproveListener(EventListener<ColorChooserApproveEvent>
                                                          listener) {
    dispatcher.removeEventListener(ColorChooserApproveEvent.class, listener);
    if (this.getBbjControl() != null
         && this.dispatcher.getListenersCount(ColorChooserApproveEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a cancel event for the colorChooser component.
   *
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser addColorChooserCancelListener(EventListener<ColorChooserCancelEvent>
                                                      listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(ColorChooserCancelEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(ColorChooserCancelEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addColorChooserCancelListener method.
   *
   * @see ColorChooser#addColorChooserCancelListener(EventListener)
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser onCancel(EventListener<ColorChooserCancelEvent> listener) {
    return addColorChooserCancelListener(listener);
  }

  /**
   * Removes a cancel event from the colorchooser component.
   *
   * @param listener the event listener to be removed
   * @return The colorChooser itself
   */
  public ColorChooser removeColorChooserCancelListener(EventListener<ColorChooserCancelEvent>
                                                         listener) {
    dispatcher.removeEventListener(ColorChooserCancelEvent.class, listener);
    if (this.getBbjControl() != null
         && this.dispatcher.getListenersCount(ColorChooserCancelEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a change event for the colorChooser component.
   *
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser addColorChooserChangeListener(EventListener<ColorChooserChangeEvent>
                                                      listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(ColorChooserChangeEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    return this;
  }

  /**
   * Alias for the addColorChooserChangeListener method.
   *
   * @see ColorChooser#addColorChooserChangeListener(EventListener)
   * @param listener the event listener to be added
   * @return The colorChooser itself
   */
  public ColorChooser onChange(EventListener<ColorChooserChangeEvent> listener) {
    return addColorChooserChangeListener(listener);
  }

  /**
   * Removes a change event from the colorchooser component.
   *
   * @param listener the event listener to be removed
   * @return The colorChooser itself
   */
  public ColorChooser removeColorChooserChangeListener(EventListener<ColorChooserChangeEvent>
                                                         listener) {
    dispatcher.removeEventListener(ColorChooserChangeEvent.class, listener);
    if (this.getBbjControl() != null
         && this.dispatcher.getListenersCount(ColorChooserChangeEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a focus event for the ColorChooser component.
   *
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser addFocusListener(EventListener<FocusEvent> listener) {
    if (this.getBbjControl() != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addFocusListener method.
   *
   * @see ColorChooser#addFocusListener(EventListener)
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  /**
   * Removes a focus event from the ColorChooser component.
   *
   * @param listener the event listener to be removed
   * @return The ColorChooser itself
   */
  public ColorChooser removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.getBbjControl() != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a blur event for the ColorChooser component.
   *
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser addBlurListener(EventListener<BlurEvent> listener) {
    if (this.getBbjControl() != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.setCallback();
    }
    dispatcher.addEventListener(BlurEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addBlurListener method.
   *
   * @see ColorChooser#addBlurListener(EventListener)
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser onBlur(EventListener<BlurEvent> listener) {
    return addBlurListener(listener);
  }

  /**
   * Removes a blur event from the ColorChooser component.
   *
   * @param listener the event listener to be removed
   * @return The ColorChooser itself
   */
  public ColorChooser removeBlurListener(EventListener<BlurEvent> listener) {
    dispatcher.removeEventListener(BlurEvent.class, listener);
    if (this.getBbjControl() != null && this.dispatcher.getListenersCount(BlurEvent.class) == 0) {
      this.blurEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseEnter event for the ColorChooser component.
   *
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseEnterListener method.
   *
   * @see ColorChooser#addMouseEnterListener(EventListener)
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  /**
   * Removes a MouseEnter event from the ColorChooser component.
   *
   * @param listener the event listener to be removed
   * @return The ColorChooser itself
   */
  public ColorChooser removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the ColorChooser component.
   *
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser addMouseExitListener(EventListener<MouseExitEvent> listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseExitEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addMouseExitListener method.
   *
   * @see ColorChooser #addMouseExitListener(EventListener)
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser onMouseExit(EventListener<MouseExitEvent> listener) {
    return addMouseExitListener(listener);
  }

  /**
   * Removes a MouseExit event from the ColorChooser component.
   *
   * @param listener the event listener to be removed
   * @return The ColorChooser itself
   */
  public ColorChooser removeMouseExitListener(EventListener<MouseExitEvent> listener) {
    dispatcher.removeEventListener(MouseExitEvent.class, listener);
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(MouseExitEvent.class) == 0) {
      this.mouseExitEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Adds a MouseExit event for the ColorChooser component.
   *
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser addRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.setCallback();
    }
    dispatcher.addEventListener(RightMouseDownEvent.class, listener);
    return this;
  }

  /**
   * Alias for the addRightMouseDownListener method.
   *
   * @see ColorChooser #addRightMouseDownListener(EventListener)
   * @param listener the event listener to be added
   * @return The ColorChooser itself
   */
  public ColorChooser onRightMouseDown(EventListener<RightMouseDownEvent> listener) {
    return addRightMouseDownListener(listener);
  }

  /**
   * Removes a RightMouseDown event from the ColorChooser component.
   *
   * @param listener the event listener to be removed
   * @return The ColorChooser itself
   */
  public ColorChooser removeRightMouseDownListener(EventListener<RightMouseDownEvent> listener) {
    dispatcher.removeEventListener(RightMouseDownEvent.class, listener);
    if (this.getBbjControl() != null
          && this.dispatcher.getListenersCount(RightMouseDownEvent.class) == 0) {
      this.rightMouseDownEventSink.removeCallback();
    }
    return this;
  }

  /**
   * Sets the visibility of the BBjColorChooser's preview panel.
   *
   * @param visible the boolean value
   * @return The ColorChooser itself
   */
  public ColorChooser setPreviewPanelVisible(boolean visible) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setPreviewPanelVisible(visible);
        this.isPreviewPanelVisible = visible;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Determines if the ColorChooser's preview panel is showing.
   *
   * @return checks if preview panel is visible.
   */
  public boolean isPreviewPanelVisible() {
    if (getBbjControl() != null) {
      try {
        getBbjControl().isPreviewPanelVisible();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this.isPreviewPanelVisible;
  }

  /**
   * Programmatically depresses the ColorChooser's [OK] button.
   *
   * @return The ColorChooser itself
   */
  public ColorChooser approveSelection() {
    if (getBbjControl() != null) {
      try {
        getBbjControl().approveSelection();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Sets the text of the ColorChooser's [OK] button.
   *
   * @param approveText [OK] button's text
   * @return the ColorChooser itself
   */
  public ColorChooser setApproveButtonText(String approveText) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setApproveButtonText(approveText);
        this.approveText = approveText;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * ColorChooser's approve button text, which by default is "OK."
   *
   * @return approve button text
   */
  public String getApproveButtonText() {
    return this.approveText;
  }

  /**
   * Sets the text of the ColorChooser's [Cancel] button.
   *
   * @param cancelText [Cancel] button's text
   * @return The ColorChooser itself
   */
  public ColorChooser setCancelButtonText(String cancelText) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setCancelButtonText(cancelText);
        this.cancelText = cancelText;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * ColorChooser's cancel button text, which by default is "Cancel."
   *
   * @return cancel button text
   */
  public String getCancelButtonText() {
    return this.cancelText;
  }

  /**
   * Programmatically depresses the ColorChooser's [Cancel] button.
   *
   * @return The ColorChooser itself
   */
  public ColorChooser cancelSelection() {
    if (getBbjControl() != null) {
      try {
        getBbjControl().cancelSelection();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Determine if the ColorChooser's approve and cancel buttons are visible.
   *
   * @return true if visible, false otherwise
   */
  public boolean getControlButtonsAreShown() {
    return areButtonsShown;
  }

  /**
   * Show or hide the ColorChooser's [OK] and [Cancel] buttons´.
   *
   * @param areShown boolean value
   * @return The ColorChooser itself
   */
  public ColorChooser setControlButtonsAreShown(Boolean areShown) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setControlButtonsAreShown(areShown);
        areButtonsShown = areShown;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public HasFocus focus() {
    super.focusComponent();
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public Boolean isTabTraversable() {
    if (getBbjControl() != null) {
      try {
        getBbjControl().isTabTraversable();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this.tabTraversable;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public TabTraversable setTabTraversable(Boolean traversable) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setTabTraversable(traversable);
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  private BBjColorChooser getBbjControl() {
    try {
      return (BBjColorChooser) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Sets the currently displayed color in the ColorChooser.
   *
   * @param color the value of displayable color.
   * @return the ColorChooser itself
   */
  public ColorChooser setColor(Color color) {
    if (getBbjControl() != null) {
      try {
        getBbjControl().setColor(new BBjColor(color));
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Sets the currently displayed color in the ColorChooser.
   *
   * @param red the value of red color.
   * @param green the value of green color.
   * @param blue the value of blue color.
   * @return the ColorChooser itself
   */
  public ColorChooser setRgbColor(float red, float green, float blue) {
    if (getBbjControl() != null) {
      try {
        Color rgbColor = new Color(red, green, blue);
        getBbjControl().setColor(new BBjColor(rgbColor));
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Sets the currently displayed color in the ColorChooser.
   *
   * @param red the value of red color.
   * @param green the value of green color.
   * @param blue the value of blue color.
   * @param alpha the values in the range (0.0 - 1.0)
   * @return the ColorChooser itself
   */
  public ColorChooser setRgbaColor(float red, float green, float blue, float alpha) {
    if (getBbjControl() != null) {
      try {
        Color rgbColor = new Color(red, green, blue, alpha);
        getBbjControl().setColor(new BBjColor(rgbColor));
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * Sets the currently displayed color in the ColorChooser.
   *
   * @param hexCode characters representing each red, green and blue colors in hexadecimal.
   * @return the ColorChooser itself
   */
  public ColorChooser setHexColor(String hexCode) {
    if (getBbjControl() != null) {
      try {
        int hexRed = Integer.valueOf(hexCode.substring(0, 2), 16);
        int hexGreen = Integer.valueOf(hexCode.substring(2, 4), 16);
        int hexBlue = Integer.valueOf(hexCode.substring(4, 6), 16);
        Color convertedColor = new Color(hexRed, hexGreen, hexBlue);
        getBbjControl().setColor(new BBjColor(convertedColor));
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  /**
   * The current color selection in the ColorChooser.
   *
   * @return the current color selection in the ColorChooser.
   */
  public Color getColor() {
    if (getBbjControl() != null) {
      try {
        BBjColor b = getBbjControl().getColor();
        return new Color(b.getRed(), b.getGreen(), b.getBlue());
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return new Color(0, 0, 0);
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setText(String text) {
    super.setText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @ExcludeFromJacocoGeneratedReport
  @Override
  public ColorChooser removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  public ColorChooser setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  public ColorChooser setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp())) {
      throw new IllegalAccessException("catchUp cannot be called twice");
    }
    super.catchUp();

    if (Boolean.FALSE.equals(this.tabTraversable)) {
      this.setTabTraversable(this.tabTraversable);
    }
  }
}
