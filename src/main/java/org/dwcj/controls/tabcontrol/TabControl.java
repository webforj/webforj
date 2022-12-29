package org.dwcj.controls.tabcontrol;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;

public final class TabControl extends AbstractDwcControl {

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public enum Theme{
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
            Environment.logError(e);
        }
    }



    @Override
    public TabControl setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public TabControl setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public TabControl setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public TabControl setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TabControl setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public TabControl setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public TabControl setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public TabControl addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public TabControl removeClassName(String selector) {
        super.removeClassName(selector);
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
