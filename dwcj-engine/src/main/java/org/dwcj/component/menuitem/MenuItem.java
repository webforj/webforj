package org.dwcj.component.menuitem;

import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;

import java.nio.charset.StandardCharsets;

import org.dwcj.Environment;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasEnable;
import org.dwcj.component.popupmenu.PopupMenu;
import org.dwcj.component.window.AbstractWindow;

public class MenuItem extends AbstractDwcComponent implements HasEnable{

  private BBjMenuItem bbjMenuItem;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
  }

  public Menu getParentMenu() {
    try {
      return (Menu) bbjMenuItem.getParentMenu();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  public PopupMenu getParentPopupMenu() {
    try {
      return (PopupMenu) bbjMenuItem.getParentPopupMenu();
    } catch (BBjException e) {
      Environment.logError(e);
      return null;
    }
  }

  public void setAccelerator(String accel) {
    try {
      bbjMenuItem.setAccelerator(accel.getBytes(StandardCharsets.UTF_8));
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }



  @Override
  public MenuItem setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public MenuItem setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public MenuItem setEnabled(boolean enabled) {
    super.setComponentEnabled(enabled);
    return this;
  }

  @Override 
  public boolean isEnabled(){
    return super.isComponentEnabled();
  }
  @Override
  public MenuItem setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public MenuItem setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public MenuItem setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public MenuItem addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public MenuItem removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }



  public MenuItem setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  @Override
  protected void create(AbstractWindow panel) {
    throw new UnsupportedOperationException("Unimplemented method 'create'");
  }

}
