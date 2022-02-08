package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.panels.IPanel;

import java.util.Iterator;
import java.util.Map;


public class ComboBox extends AbstractDwclistControl {


    public ComboBox() {
    }


    @Override
    public void create(IPanel p) {
        BBjWindow w = p.getBBjWindow();


        try {
            ctrl = w.addListButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            ctrl.setAttribute("maxRowCount", "25");
            ctrl.setAttribute("openWidth", "2500");
            populate();

        } catch (BBjException e) {
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


}
