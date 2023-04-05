package org.dwcj.component.window;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcjAppInitializeException;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * AppPanel is the core main application panel. It typically occupied the full browser real estate
 * and holds your app that consists of div container panels
 */
public class Frame extends AbstractWindow {

  public Frame() throws DwcjAppInitializeException {

    try {
      byte[] flags = new byte[] {(byte) 0x01, (byte) 0x11, (byte) 0x10, (byte) 0x88};
      BasisNumber b1 = BasisNumber.createBasisNumber(1);
      BasisNumber ctx = BasisNumber
          .createBasisNumber(Environment.getInstance().getSysGui().getAvailableContext());
      wnd = Environment.getInstance().getSysGui().addWindow(ctx, b1, b1, b1, b1, "AppPanel", flags);
      ctrl = wnd;
    } catch (NumberFormatException | BBjException e) {
      Environment.logError(e);
      throw new DwcjAppInitializeException(e);
    }

  }

  @Override
  protected void create(AbstractWindow p) {
    // empty, needs override
  }

  @Override
  public String getStyle(String property) {
    return wnd.getPanelStyle(property);
  }

  @Override
  public String getComputedStyle(String property) {
    return wnd.getPanelStyle(property);
  }

  @Override
  public Frame setStyle(String property, String value) {
    wnd.setPanelStyle(property, value);
    return this;
  }

  @Override
  public Frame setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public Frame setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public Frame setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  @Override
  public Frame setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public Frame setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public Frame setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public Frame addClassName(String selector) {
    try {
      wnd.addPanelStyle(selector);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add Frame class name", e);
    }

    return this;
  }

  @Override
  public Frame removeClassName(String selector) {
    try {
      wnd.removePanelStyle(selector);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to remove Frame class name", e);
    }

    return this;
  }
}
