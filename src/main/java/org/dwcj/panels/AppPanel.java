package org.dwcj.panels;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.google.common.primitives.Ints;
import org.dwcj.Environment;
import org.dwcj.exceptions.DwcAppInitializeException;

public class AppPanel extends AbstractDwcjPanel {

    public AppPanel() throws DwcAppInitializeException {

        try {
            BasisNumber b1 = BasisNumber.createBasisNumber(1);
            BasisNumber ctx = BasisNumber.createBasisNumber(Environment.getInstance().getSysGui().getAvailableContext());
            wnd = Environment.getInstance().getSysGui().addWindow(ctx, b1, b1, b1, b1, "AppPanel", Ints.toByteArray(0x01111088));
            ctrl = wnd;
        } catch (NumberFormatException | BBjException e) {
            e.printStackTrace();
            throw new DwcAppInitializeException(e);
        }

    }


    @Override
    public void setStyle(String property, String value) {
        wnd.setPanelStyle(property, value);
    }

    @Override
    public void create(AbstractDwcjPanel p) {

    }
}
