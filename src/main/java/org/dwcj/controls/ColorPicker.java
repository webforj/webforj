package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class ColorPicker extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

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
    public ColorPicker setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
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

    @Override
    public ColorPicker setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public ColorPicker setText(String text) {
        super.setText(text);
        return this;
    }
    
}
