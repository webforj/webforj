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
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.dwcj.utilities.BBjFunctionalityHelper;

import java.awt.*;

public final class ColorChooser extends AbstractDwcComponent implements HasEnable, HasFocus, TabTraversable {

  private BBjColorChooser bbjColorChooser;
  private BBjColor bbjColor;


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
      // todo: honor visibility flag, if set before adding the control to the form, so it's created
      // invisibly right away
      byte[] flags =
        BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
      this.setControl(w.addColorChooser(new BBjColor(getColor()), flags));
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
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
        bbjColorChooser.isTabTraversable();
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
        bbjColorChooser.setTabTraversable(traversable);
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
