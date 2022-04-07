package org.dwcj.panels;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.App;
import org.dwcj.Environment;

/**
 * This class represents a div container, which behaves as a panel and
 * can be styled and hold other divs (panels) and controls
 */
public class Div extends AbstractDwcjPanel {

    public Div() {
    }

    void create(AbstractDwcjPanel p) {
        App.consoleLog("reached create");
        BBjWindow w = p.getBBjWindow();

        try {
            byte[] flags = new byte[]{(byte) 0x00, (byte) 0x10, (byte) 0x88, (byte) 0x00};
            //todo honor visible flag if set before addition to panel
            wnd = w.addChildWindow(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags, Environment.getInstance().getSysGui().getAvailableContext());
            ctrl = wnd;
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

}




