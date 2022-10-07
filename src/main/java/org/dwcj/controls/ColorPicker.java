package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class ColorPicker extends AbstractDwcControl {

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL, XXSMALL, XXXSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    
    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addColorChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }



    @Override
    public ColorPicker setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public ColorPicker setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public ColorPicker setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public ColorPicker setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public ColorPicker setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public ColorPicker setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public ColorPicker setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public ColorPicker addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public ColorPicker removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }





    public ColorPicker setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public ColorPicker setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
    
}
