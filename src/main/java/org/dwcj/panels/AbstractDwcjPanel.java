package org.dwcj.panels;


import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.IStyleable;

public abstract class AbstractDwcjPanel extends AbstractDwcControl implements IStyleable {

    protected BBjWindow wnd;

    public void add(AbstractDwcControl ctrl) {
        ctrl.create(this);
    }

    static {
        PanelAccessor.setDefault(new PanelAccessorImpl());
    }

    /**
     * This method is only accessible through "friend" classes
     * no customer shall ever use this directly
     * @return the underlying BBjWindow
     */
    BBjWindow getBBjWindow() {
        return wnd;
    }

    @Override
    public void setStyle(String property, String value) {
        try {
            wnd.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void addClass(String selector) {
        try {
            wnd.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void removeClass(String selector) {
        try {
            wnd.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


}
