package org.dwcj.controls.panels;


import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.annotations.AnnotationProcessor;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractControl;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.exceptions.DwcAnnotationException;

import java.util.ArrayList;

/**
 * the base class for all panel implementations
 */
public abstract class AbstractDwcjPanel extends AbstractDwcControl {

    protected BBjWindow wnd;

    protected ArrayList<AbstractControl> controls = new ArrayList<>();

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
                AnnotationProcessor processor = new AnnotationProcessor();
                processor.processControlAnnotations(c);
                ControlAccessor.getDefault().create(c,this);
                controls.add(c);
            } catch (IllegalAccessException | DwcAnnotationException e) {
                Environment.logError(e);
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

    @Override
    public AbstractDwcjPanel setStyle(String property, String value) {
        if(this.wnd != null){
            wnd.setPanelStyle(property, value);
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel addClassName(String selector) {
        if(this.wnd != null){
            try {
                wnd.addPanelStyle(selector);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    @Override
    public AbstractDwcjPanel removeClassName(String selector) {
        if(this.wnd != null){
            try {
                wnd.removePanelStyle(selector);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }


}
