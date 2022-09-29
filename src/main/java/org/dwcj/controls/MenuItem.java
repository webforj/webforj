package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;

import java.nio.charset.StandardCharsets;

public class MenuItem extends AbstractDwcControl implements IThemable {

    private BBjMenuItem bbjMenuItem;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

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


    public MenuItem setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse.toString());
        return this;
    }

    @Override
    public MenuItem setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public MenuItem addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public MenuItem removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public MenuItem setID(String id){
        super.setID(id);
        return this;
    }
}
