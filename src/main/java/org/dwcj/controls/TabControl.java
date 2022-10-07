package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class TabControl extends AbstractDwcControl {

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addTabCtrl(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }



    @Override
    public TabControl setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public TabControl setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public TabControl setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public TabControl setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public TabControl setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public TabControl setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public TabControl setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public TabControl addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public TabControl removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }



    public TabControl setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }


    public TabControl setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
