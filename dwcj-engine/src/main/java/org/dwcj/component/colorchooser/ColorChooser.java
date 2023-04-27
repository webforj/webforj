package org.dwcj.component.colorchooser;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.window.AbstractWindow;

public final class ColorChooser extends AbstractDwcComponent {

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL, XXSMALL, XXXSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }


  @Override
  protected void create(AbstractWindow p) {

    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      // todo: honor visibility flag, if set before adding the control to the form, so it's created
      // invisibly right away
      ctrl = w.addColorChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1, BASISNUMBER_1);
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
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
  public ColorChooser setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
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
