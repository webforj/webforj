package org.dwcj.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

public class Div extends AbstractDwcjPanel {

    public Div() {
    }

    public void create(AbstractDwcjPanel p) {
        BBjWindow w = p.getBBjWindow();

        try {

            byte[] b = new byte[4];
            //$
            b[0] = 0;        //00
            b[1] = 16;    //10
            b[2] = -120;    //88
            b[3] = 0;        //00
            //$

            wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", b, Environment.getInstance().getSysGui().getAvailableContext());
            ctrl = wnd;
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

}




