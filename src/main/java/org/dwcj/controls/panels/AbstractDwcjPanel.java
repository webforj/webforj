package org.dwcj.controls.panels;


import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractControl;
import org.dwcj.controls.AbstractDwcControl;

/**
 * the base class for all panel implementations
 */
public abstract class AbstractDwcjPanel extends AbstractDwcControl {

    protected BBjWindow wnd;

    /**
     * add a control to the panel
     * @param ctrl the control to be added
     * @return the panel itself
     */
    public AbstractDwcjPanel add(AbstractControl ctrl) {
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
        if(this.wnd != null){
            try {
                wnd.setStyle(property, value);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel addClassName(String selector) {
        if(this.wnd != null){
            try {
                wnd.addStyle(selector);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel removeClassName(String selector) {
        if(this.wnd != null){
            try {
                wnd.removeStyle(selector);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }


}
