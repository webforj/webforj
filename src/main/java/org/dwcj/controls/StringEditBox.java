package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class StringEditBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    String mask;
    public StringEditBox() {}

    public StringEditBox(String text) {
        setText(text);
    }

    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public StringEditBox setMask(String mask){
        if (ctrl != null){
        try {
            ((BBjInputE)ctrl).setMask(mask);
        } catch (BBjException e) {
            App.consoleLog(e.getMessage());
            throw new RuntimeException(e);

        }
        }
        this.mask = mask;
        return this;
    }

    @Override
    public StringEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public StringEditBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public StringEditBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public StringEditBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public StringEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public StringEditBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();
        if (this.mask != null)
            setMask(this.mask);
    }
}
