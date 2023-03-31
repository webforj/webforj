package org.dwcj.component.colorchooser;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.AbstractDwcControl;
import org.dwcj.component.panels.AbstractPanel;

public final class ColorPicker extends AbstractDwcControl {

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL, XXSMALL, XXXSMALL
    }

    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    
    @Override
    protected void create(AbstractPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addColorChooser(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @Override
    public ColorPicker setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public ColorPicker setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public ColorPicker setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public ColorPicker setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public ColorPicker setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public ColorPicker setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public ColorPicker setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public ColorPicker addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public ColorPicker removeClassName(String selector) {
        super.removeClassName(selector);
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
