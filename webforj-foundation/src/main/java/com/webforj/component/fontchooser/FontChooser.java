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

  public FontChooser approveSelection() {
    try {
      bbjFontChooser.approveSelection();
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser cancelSelection() {
    try {
      bbjFontChooser.cancelSelection();
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public String getApproveButtonText() {
    try {
      return bbjFontChooser.getApproveButtonText();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  public String getCancelButtonText() {
    try {
      return bbjFontChooser.getCancelButtonText();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  public boolean isControlButtonsAreShown() {
    try {
      return bbjFontChooser.getControlButtonsAreShown();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return false;
    }
  }

  public boolean isFontsScaled() {
    try {
      return bbjFontChooser.getFontsScaled();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return false;
    }
  }

  public String getPreviewMessage() {
    try {
      return bbjFontChooser.getPreviewMessage();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return "";
    }
  }

  public Font getSelectedFont() {
    try {
      return (Font) bbjFontChooser.getSelectedFont();
    } catch (BBjException e) {
      // Environment.logError(e);;
      return null;
    }
  }

  public FontChooser setApproveButtonText(String text) {
    try {
      bbjFontChooser.setApproveButtonText(text);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser setCancelButtonText(String text) {
    try {
      bbjFontChooser.setCancelButtonText(text);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser setControlButtonsAreShown(boolean show) {
    try {
      bbjFontChooser.setControlButtonsAreShown(show);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser setFontsScaled(boolean scale) {
    try {
      bbjFontChooser.setFontsScaled(scale);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser setPreviewMessage(String message) {
    try {
      bbjFontChooser.setPreviewMessage(message);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser setSelectedFont(Font font) {
    try {
      bbjFontChooser.setSelectedFont((BBjFont) font);
    } catch (BBjException e) {
      // Environment.logError(e);;
    }
    return this;
  }

  public FontChooser onFontChooserApprove(Consumer<FontChooserApproveEvent> callback) {
    if (this.fontChooserApproveEventSink == null)
      this.fontChooserApproveEventSink = new FontChooserApproveEventSink(this, callback);
    else
      this.fontChooserApproveEventSink.addCallback(callback);
    return this;
  }

  public FontChooser onFontChooserCancel(Consumer<FontChooserCancelEvent> callback) {
    if (this.fontChooserCancelEventSink == null)
      this.fontChooserCancelEventSink = new FontChooserCancelEventSink(this, callback);
    else
      this.fontChooserCancelEventSink.addCallback(callback);
    return this;
  }

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
