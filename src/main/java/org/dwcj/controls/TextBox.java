package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.panels.IPanel;


public class TextBox extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private String sText = "";

    public TextBox() {
    }

    public TextBox(String text) {
        this.sText = text;
    }

    @Override
    public void create(IPanel p) {
        BBjWindow w = p.getBBjWindow();

        try {
            ctrl = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, sText);
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }


    @Override
    public void setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
    }

    @Override
    public void setStyle(String property, String value) {
        super.setControlStyle(property, value);
    }

    @Override
    public void addClass(String selector) {
        super.addControlCssClass(selector);
    }

    @Override
    public void removeClass(String selector) {
        super.removeControlCssClass(selector);
    }

    @Override
    public void setTheme(Theme theme) {
        super.setControlTheme(theme);
    }
}
