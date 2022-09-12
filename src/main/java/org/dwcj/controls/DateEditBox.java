package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class DateEditBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {
    
    public DateEditBox(){}

    public DateEditBox(String text) { setText(text); }
    
    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputD(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void calendar() {
        ((BBjInputD) this.ctrl).calendar();
    }

    @Override
    public DateEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public DateEditBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public DateEditBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public DateEditBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public DateEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    } 
}
