package org.dwcj.component.colorchooser;

import com.basis.bbj.proxies.sysgui.BBjColorChooser;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.sysgui.BBjColor;
import org.dwcj.Environment;
import org.dwcj.bridge.ComponentAccessor;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.HasFocus;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.FocusEvent;
import org.dwcj.component.event.MouseEnterEvent;
import org.dwcj.component.event.sink.FocusEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

import java.awt.*;

public final class ColorChooser extends AbstractDwcComponent implements HasEnable, HasFocus, TabTraversable {
  private boolean areButtonsShown = true;
  private String approveText = "OK";
  private String cancelText = "Cancel";
  private boolean isPreviewPanelVisible = true;
  private EventDispatcher dispatcher = new EventDispatcher();
  private MouseEnterEventSink mouseEnterEventSink;
  private MouseExitEventSink mouseExitEventSink;
  private RightMouseDownEventSink rightMouseDownEventSink;
  private FocusEventSink focusEventSink;


  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL, XXSMALL, XXXSMALL;
  }

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
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  public ColorChooser addFocusListener(EventListener<FocusEvent> listener) {
    if (this.getBBjControl() != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.setCallback();
    }
    dispatcher.addEventListener(FocusEvent.class, listener);
    return this;
  }

  public ColorChooser onFocus(EventListener<FocusEvent> listener) {
    return addFocusListener(listener);
  }

  public ColorChooser removeFocusListener(EventListener<FocusEvent> listener) {
    dispatcher.removeEventListener(FocusEvent.class, listener);
    if (this.getBBjControl() != null && this.dispatcher.getListenersCount(FocusEvent.class) == 0) {
      this.focusEventSink.removeCallback();
    }
    return this;
  }

  public ColorChooser addMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    if (this.getBBjControl() != null &&
      this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.setCallback();
    }
    dispatcher.addEventListener(MouseEnterEvent.class, listener);
    return this;
  }

  public ColorChooser onMouseEnter(EventListener<MouseEnterEvent> listener) {
    return addMouseEnterListener(listener);
  }

  public ColorChooser removeMouseEnterListener(EventListener<MouseEnterEvent> listener) {
    dispatcher.removeEventListener(MouseEnterEvent.class, listener);
    if (this.getBBjControl() != null && this.dispatcher.getListenersCount(MouseEnterEvent.class) == 0) {
      this.mouseEnterEventSink.removeCallback();
    }
    return this;
  }

  public ColorChooser setPreviewPanelVisible(boolean visible) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setPreviewPanelVisible(visible);
        this.isPreviewPanelVisible = visible;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  public boolean getPreviewPanelVisible() {
    if (getBBjControl() != null) {
      try {
        getBBjControl().isPreviewPanelVisible();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this.isPreviewPanelVisible;
  }

  public ColorChooser approveSelection() {
    if (getBBjControl() != null) {
      try {
        getBBjControl().approveSelection();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  public ColorChooser setApproveButtonText(String approveText) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setApproveButtonText(approveText);
        this.approveText = approveText;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  public String getApproveButtonText() {
    return this.approveText;
  }

  public ColorChooser setCancelButtonText(String cancelText) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setCancelButtonText(cancelText);
        this.cancelText = cancelText;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  public String getCancelButtonText() {
    return this.cancelText;
  }

  public ColorChooser cancelSelection() {
    if (getBBjControl() != null) {
      try {
        getBBjControl().cancelSelection();
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  public boolean getControlButtonsAreShown() {
    return areButtonsShown;
  }

  public ColorChooser setControlButtonsAreShown(Boolean areShown) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setControlButtonsAreShown(areShown);
        areButtonsShown = areShown;
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    return this;
  }

  @Override
  public HasFocus focus() {
    super.focusComponent();
    return this;
  }

  @Override
  public Boolean isTabTraversable() {
    if (getBBjControl() != null) {
      try {
        getBBjControl().isTabTraversable();
      } catch(BBjException e) {
        throw new RuntimeException();
      }
    }
    return this.tabTraversable;
  }

  @Override
  public TabTraversable setTabTraversable(Boolean traversable) {
    if (getBBjControl() != null) {
      try {
        getBBjControl().setTabTraversable(traversable);
      } catch (BBjException e) {
        throw new RuntimeException();
      }
    }
    this.tabTraversable = traversable;
    return this;
  }

  private BBjColorChooser getBBjControl() {
    try {
      return (BBjColorChooser) ComponentAccessor.getDefault().getBBjControl(this);
    } catch (IllegalAccessException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  public ColorChooser setColor(Color color) {
    if(getBBjControl() != null) {
      try {
        getBBjControl().setColor(new BBjColor(color));
      }catch (BBjException e){
        throw new RuntimeException();
      }
    }
    return this;
  }

  public Color getColor() {
    if(getBBjControl() != null) {
      try {
        BBjColor b = getBBjControl().getColor();
        return new Color(b.getRed(), b.getGreen(), b.getBlue());
      } catch(BBjException e) {
        throw new RuntimeException();
      }
    }
    return new Color(0, 0, 0);
  }

  @Override
  public ColorChooser setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public ColorChooser setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public ColorChooser setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override
  public boolean isEnabled() {
    return super.isComponentEnabled();
  }

  @Override
  public ColorChooser setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public ColorChooser setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public ColorChooser setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public ColorChooser addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public ColorChooser removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  public ColorChooser setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public ColorChooser setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }

}
