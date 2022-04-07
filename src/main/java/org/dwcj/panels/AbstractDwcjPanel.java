package org.dwcj.panels;


import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.IControl;
import org.dwcj.controls.IStyleable;

/**
 * the base class for all panel implementations
 */
public abstract class AbstractDwcjPanel extends AbstractDwcControl implements IStyleable {

    protected BBjWindow wnd;

    /**
     * add a control to the panel
     * @param ctrl the control to be added
     * @return the panel itself
     */
    public AbstractDwcjPanel add(AbstractDwcControl ctrl) {
        try {
            ControlAccessor.getDefault().create(ctrl,this);
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return this;
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
    public AbstractDwcjPanel setStyle(String property, String value) {
        try {
            wnd.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel addClass(String selector) {
        try {
            wnd.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel removeClass(String selector) {
        try {
            wnd.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }


}
