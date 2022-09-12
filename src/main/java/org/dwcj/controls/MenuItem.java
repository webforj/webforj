package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;

import java.nio.charset.StandardCharsets;

public class MenuItem extends AbstractDwcControl implements IStyleable, IExpansible, IThemable {

    private BBjMenuItem bbjMenuItem;

    public Menu getParentMenu() {
        try {
            return (Menu) bbjMenuItem.getParentMenu();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public PopupMenu getParentPopupMenu() {
        try {
            return (PopupMenu) bbjMenuItem.getParentPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public void setAccelerator(String accel) {
        try {
            bbjMenuItem.setAccelerator(accel.getBytes(StandardCharsets.UTF_8));
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
