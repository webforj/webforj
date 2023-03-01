package org.dwcj.controls;

import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.panels.AbstractPanel;
import org.dwcj.interfaces.Control;
import org.dwcj.interfaces.HasDestroy;

import java.util.HashMap;
import java.util.Map;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;

/**
 * Abstract base class for all engine controls. Implements default behaviors
 * for the implemented interface methods. Extended by AbstractDwcControl.
 */

public abstract class AbstractControl implements Control, HasDestroy {

    /*
     * Underlying BBj control
     */
    protected BBjControl ctrl;

    /*
     * Members responsible for tracking ID attribute and user data
     */
    protected String elementId = "";
    protected final Map<String, Object> userData = new HashMap<>();

    /*
     * Used by catchUp() method to ensure a single execution of
     * the function.
     */
    private Boolean caughtUp = false;

    /*
     * Used to track whether or not the control that has been flagged
     * for destruction
     */
    protected Boolean destroyed = false;

    static {
        ControlAccessor.setDefault(new CtrlAccessorImpl());
    }

    /**
     * Create the object on a panel p. The preferred way of creating an object is
     * using the
     * Panel::add(Control) method, instead of this
     * 
     * @param p the panel to add this control on
     */
    protected void create(AbstractPanel p) {
    }

    /**
     * This method returns the underlying original BBj control
     * It's package private and can only be accessed through the ControlAccessor
     * No API user / customer shall ever work directly with BBj controls
     *
     * @return the underlying BBj control
     */
    BBjControl getControl() {
        return this.ctrl;
    }

    @Override
    public String getId() {
        if (this.ctrl != null) {
            try {
                return ctrl.getAttribute("id");
            } catch (BBjException e) {
                Environment.logError(e);
            }
        } else if (!(this.elementId.equals(""))) {
            return this.elementId;
        }
        return "";
    }

    @Override
    public AbstractControl setId(String elementId) {
        if (this.ctrl != null) {
            try {
                this.ctrl.setAttribute("id", elementId);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.elementId = elementId;
        return this;
    }

    public Object getUserData(String key) {
        return this.userData.get(key);
    }

    public AbstractControl setUserData(String key, Object data) {
        this.userData.put(key, data);
        return this;
    }

    public Boolean getCaughtUp() {
        return this.caughtUp;
    }

    /**
     * The catchUp method is used to replay attributes and settings that the API
     * user might have
     * added to a control before its creation. A control is not created before it's
     * added
     * to a panel. Anything that is added between instantiation of a control and its
     * addition to a panel
     * has to be recorded and replayed in this method
     *
     * @throws IllegalAccessException - thrown if an attempt is made to call this
     *                                method more than once
     */
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {

        if (Boolean.TRUE.equals(this.caughtUp)){
            throw new IllegalAccessException("catchUp cannot be called twice");
        } 

        if (!(this.elementId.equals(""))) {
            this.setId(this.elementId);
        }

        this.caughtUp = true;
    }

    @Override
    public void destroy(){
        this.destroyed = true;
    }

    @Override
    public Boolean isDestroyed(){
        return this.destroyed;
    }
}
