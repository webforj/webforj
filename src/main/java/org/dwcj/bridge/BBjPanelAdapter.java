package org.dwcj.bridge;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.panels.AbstractDwcjPanel;

/**
 * ********** IMPORTANT: ****************+
 * This class is only needed for using DWCJ Controls from BBj code. It has no relevance to the Java development with DWCJ.
 * The BBjPanelAdapter converts a BBjWindow into an AbstractDwcPanel so that DWCJ Controls can
 * be added to code that is written in the BBj language.
 */
public class BBjPanelAdapter extends AbstractDwcjPanel {

    public BBjPanelAdapter(BBjWindow w){
        this.wnd = w;
    }

    public AbstractDwcjPanel add(AbstractDwcControl ctrl) {
        try {
            ControlAccessor.getDefault().create(ctrl,this);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return this;
    }

}
