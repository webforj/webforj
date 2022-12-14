package org.dwcj.controls.menuitem;

import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;

import java.nio.charset.StandardCharsets;

import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.popupmenu.PopupMenu;

public class MenuItem extends AbstractDwcControl {

    private BBjMenuItem bbjMenuItem;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
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



    @Override
    public MenuItem setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public MenuItem setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public MenuItem setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public MenuItem setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public MenuItem setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public MenuItem setId(String id){
        super.setId(id);
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

}
