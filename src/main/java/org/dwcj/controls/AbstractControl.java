package org.dwcj.controls;

import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.Control;

import java.util.HashMap;
import java.util.Map;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;

/**
 * Abstract base class for all engine controls. Implements default behaviors
 * for the implemented interface methods. Extended by AbstractDwcControl.
 */

public abstract class AbstractControl implements Control {

    /*
     * Underlying BBj control
     */
    protected BBjControl ctrl;

    /*
     * Members responsible for tracking ID attribute and user data
     */
    protected String Id = "";
    protected final Map<String, Object> userData = new HashMap<>();

    /*
     * Used by catchUp() method to ensure a single execution of
     * the function.
     */
    private Boolean caughtUp = false;

    static {
        ControlAccessor.setDefault(new CtrlAccessorImpl());
    }

    /**
     * Create the object on a panel p. The preferred way of creating an object is
     * using the
     * Panel::add(Control) method, instead of this
     * 
     * @param p
     */
    protected void create(AbstractDwcjPanel p) {
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
                e.printStackTrace();
            }
        } else if (!(this.Id.equals(""))) {
            return this.Id;
        }
        return "";
    }

    @Override
    public AbstractControl setId(String id) {
        if (this.ctrl != null) {
            try {
                this.ctrl.setAttribute("id", id);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.Id = id;
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

        if (Boolean.TRUE.equals((this.caughtUp))){
            throw new IllegalAccessException("catchUp cannot be called twice");
        } 

        if (!(this.Id.equals(""))) {
            this.setId(this.Id);
        }

        this.caughtUp = true;
    }
}
