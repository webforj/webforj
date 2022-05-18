package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;


public final class    TextBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    public TextBox() {
    }

    public TextBox(String text) {
        setText(text);
    }

    @Override
    void create(AbstractDwcjPanel p) {


        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    @Override
    public TextBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public TextBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public TextBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public TextBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public TextBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
