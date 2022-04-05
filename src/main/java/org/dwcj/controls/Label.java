package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public class Label extends AbstractDwcControl implements IStyleable {

    private String sText = "";

    public Label() {
    }

    public Label(String text) {
        this.sText = text;
    }

    @Override
    public void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addStaticText(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, sText);
        } catch (Exception e) {
            e.printStackTrace();
        }

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

    public void setText(String string) {
        try {
            ctrl.setText(string);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }
}
