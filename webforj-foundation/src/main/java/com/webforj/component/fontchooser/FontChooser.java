package com.webforj.component.fontchooser;

import com.basis.bbj.proxies.sysgui.BBjFont;
import com.basis.bbj.proxies.sysgui.BBjFontChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.LegacyDwcComponent;
import com.webforj.component.fontchooser.event.FontChooserApproveEvent;
import com.webforj.component.fontchooser.event.FontChooserCancelEvent;
import com.webforj.component.fontchooser.event.FontChooserChangeEvent;
import com.webforj.component.fontchooser.sink.FontChooserApproveEventSink;
import com.webforj.component.fontchooser.sink.FontChooserCancelEventSink;
import com.webforj.component.fontchooser.sink.FontChooserChangeEventSink;
import com.webforj.component.window.Window;
import com.webforj.concern.legacy.LegacyHasEnable;
import java.awt.*;
import java.util.function.Consumer;


/**
 * A component that lets the user pick a font.
 *
 * @author Stephan Wald
 * @since 0.006
 */
public final class FontChooser extends LegacyDwcComponent implements LegacyHasEnable {

  private FontChooserApproveEventSink fontChooserApproveEventSink;

  private FontChooserCancelEventSink fontChooserCancelEventSink;

  private FontChooserChangeEventSink fontChooserChangeEventSink;

  private BBjFontChooser bbjFontChooser;

  @Override
  protected void onCreate(Window p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      // todo: honor visbility flag
      control = w.addFontChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1);
      bbjFontChooser = (BBjFontChooser) control;
      onAttach();
    } catch (Exception e) {
      // Environment.logError(e);;
    }
  }

  /**
   * Approves the current font selection.
   *
   * @return the component itself
   */
  public FontChooser approveSelection() {
    try {
      bbjFontChooser.approveSelection();
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Cancels the current font selection.
   *
   * @return the component itself
   */
  public FontChooser cancelSelection() {
    try {
      bbjFontChooser.cancelSelection();
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Gets the text of the approve button.
   *
   * @return the approve button text
   */
  public String getApproveButtonText() {
    try {
      return bbjFontChooser.getApproveButtonText();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  /**
   * Gets the text of the cancel button.
   *
   * @return the cancel button text
   */
  public String getCancelButtonText() {
    try {
      return bbjFontChooser.getCancelButtonText();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  /**
   * Checks whether the control buttons are shown.
   *
   * @return true if the control buttons are shown, false otherwise
   */
  public boolean isControlButtonsAreShown() {
    try {
      return bbjFontChooser.getControlButtonsAreShown();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return false;
    }
  }

  /**
   * Checks whether the fonts are scaled.
   *
   * @return true if the fonts are scaled, false otherwise
   */
  public boolean isFontsScaled() {
    try {
      return bbjFontChooser.getFontsScaled();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return false;
    }
  }

  /**
   * Gets the preview message.
   *
   * @return the preview message
   */
  public String getPreviewMessage() {
    try {
      return bbjFontChooser.getPreviewMessage();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  /**
   * Gets the selected font.
   *
   * @return the selected font
   */
  public Font getSelectedFont() {
    try {
      return (Font) bbjFontChooser.getSelectedFont();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return null;
    }
  }

  /**
   * Sets the text of the approve button.
   *
   * @param text the approve button text
   * @return the component itself
   */
  public FontChooser setApproveButtonText(String text) {
    try {
      bbjFontChooser.setApproveButtonText(text);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Sets the text of the cancel button.
   *
   * @param text the cancel button text
   * @return the component itself
   */
  public FontChooser setCancelButtonText(String text) {
    try {
      bbjFontChooser.setCancelButtonText(text);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Sets whether the control buttons are shown.
   *
   * @param show true to show the control buttons, false to hide them
   * @return the component itself
   */
  public FontChooser setControlButtonsAreShown(boolean show) {
    try {
      bbjFontChooser.setControlButtonsAreShown(show);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Sets whether the fonts are scaled.
   *
   * @param scale true to scale the fonts, false otherwise
   * @return the component itself
   */
  public FontChooser setFontsScaled(boolean scale) {
    try {
      bbjFontChooser.setFontsScaled(scale);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Sets the preview message.
   *
   * @param message the preview message
   * @return the component itself
   */
  public FontChooser setPreviewMessage(String message) {
    try {
      bbjFontChooser.setPreviewMessage(message);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Sets the selected font.
   *
   * @param font the font to select
   * @return the component itself
   */
  public FontChooser setSelectedFont(Font font) {
    try {
      bbjFontChooser.setSelectedFont((BBjFont) font);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  /**
   * Adds a listener for the approve event.
   *
   * @param callback the listener
   * @return the component itself
   */
  public FontChooser onFontChooserApprove(Consumer<FontChooserApproveEvent> callback) {
    if (this.fontChooserApproveEventSink == null)
      this.fontChooserApproveEventSink = new FontChooserApproveEventSink(this, callback);
    else
      this.fontChooserApproveEventSink.addCallback(callback);
    return this;
  }

  /**
   * Adds a listener for the cancel event.
   *
   * @param callback the listener
   * @return the component itself
   */
  public FontChooser onFontChooserCancel(Consumer<FontChooserCancelEvent> callback) {
    if (this.fontChooserCancelEventSink == null)
      this.fontChooserCancelEventSink = new FontChooserCancelEventSink(this, callback);
    else
      this.fontChooserCancelEventSink.addCallback(callback);
    return this;
  }

  /**
   * Adds a listener for the change event.
   *
   * @param callback the listener
   * @return the component itself
   */
  public FontChooser onFontChooserChange(Consumer<FontChooserChangeEvent> callback) {
    if (this.fontChooserChangeEventSink == null)
      this.fontChooserChangeEventSink = new FontChooserChangeEventSink(this, callback);
    else
      this.fontChooserChangeEventSink.addCallback(callback);
    return this;
  }

  @Override
  public FontChooser setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public FontChooser setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public FontChooser setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public FontChooser setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public FontChooser setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public FontChooser setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public FontChooser addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public FontChooser removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }
}
