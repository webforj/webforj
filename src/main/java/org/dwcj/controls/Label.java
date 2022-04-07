package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public class Label extends AbstractDwcControl implements IStyleable {

    public Label() {
    }

    public Label(String text) {
        setText(text);
    }

    @Override
    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    @Override
    public Label setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public Label addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Label removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
