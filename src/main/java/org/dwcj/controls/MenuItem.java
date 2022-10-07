package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.resource.Menu;
import com.basis.startup.type.BBjException;

import java.nio.charset.StandardCharsets;

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
        super.setControlText(text);
        return this;
    }

    @Override
    public MenuItem setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public MenuItem setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public MenuItem setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public MenuItem setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public MenuItem setID(String id){
        super.setControlID(id);
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




    public MenuItem setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

}
