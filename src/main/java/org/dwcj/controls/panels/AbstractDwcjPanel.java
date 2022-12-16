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
     * Used to add controls to a panel. Multiple controls can be passed to this
     * function, and will be added in the order the arguments are passed 
     * (arg0 added first, arg1 second, etc...)
     * @param ctrl the control(s) to be added
     * @return the panel itself
     */
    public AbstractDwcjPanel add(AbstractControl ...ctrl) {
        for(AbstractControl c: ctrl){
            try {
                ControlAccessor.getDefault().create(c,this);
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
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

    
    public AbstractDwcjPanel addClass(String selector) {
        if(this.wnd != null){
            try {
                wnd.addStyle(selector);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    
    public AbstractDwcjPanel removeClass(String selector) {
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
