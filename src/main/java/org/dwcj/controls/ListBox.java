package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Iterator;
import java.util.Map;

public final class ListBox extends AbstractDwclistControl implements IThemable, IExpansible {

    public ListBox() {}

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addListBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("max-row-count", "25");
            ctrl.setAttribute("open-width", "2500");
            ctrl.setAttribute("button-height", "auto");
            populate();
            catchUp();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public ListBox setItems(Map<Object, String> values) {
        this.values = values;
        populate();
        return this;
    }

    @SuppressWarnings("unchecked")
    private void populate() {
        if (values != null && ctrl != null) try {
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
    public ListBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public ListBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
