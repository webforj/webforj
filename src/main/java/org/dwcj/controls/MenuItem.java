package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.nio.charset.StandardCharsets;

public class MenuItem extends AbstractDwcControl implements IStyleable, IExpansible, IThemable {

    private BBjMenuItem menuItem;

    public MenuItem() {}

    public Menu getParentMenu() {
        try {
            return (Menu) menuItem.getParentMenu();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public PopupMenu getParentPopupMenu() {
        try {
            return (PopupMenu) menuItem.getParentPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setAccelerator(String accel) {
        try {
            menuItem.setAccelerator(accel.getBytes(StandardCharsets.UTF_8));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


    @Override
    public IExpansible setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
