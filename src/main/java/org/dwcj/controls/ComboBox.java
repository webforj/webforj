package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;


public class ComboBox extends AbstractDwclistControl implements IStyleable, IThemable, IExpansible {


    public ComboBox() {
    }


    @Override
    public void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = w.addListButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height","auto");
            populate();

        } catch (Exception e) {
            e.printStackTrace();
        }

    }


    public void setItems(Map<Object, String> values) {
        this.values = values;
        populate();
    }


    @SuppressWarnings("unchecked")
    private void populate() {
        if (values != null && ctrl != null)

            try {
                BBjListButton cb = (BBjListButton) ctrl;
                cb.removeAllItems();
                BBjVector v = new BBjVector();
                Iterator<Object> it = values.keySet().iterator();
                while (it.hasNext()) {
                    v.add(values.get(it.next()));
                }
                cb.insertItems(0, v);
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
