package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.*;

/**
 * The base class for all controls and panels
 */
public abstract class AbstractDwcControl implements IControl {


    protected final static BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
    protected final static BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
    protected final static BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

    static {
        ControlAccessor.setDefault(new CtrlAccessorImpl());
    }

    protected BBjControl ctrl;
    private String text = "";
    private IThemable.Theme theme;
    private IExpansible.Expanse expanse;
    private Map<String, String> attributes = new HashMap<String, String>();
    private Map<String, String> styles = new HashMap<String, String>();
    private List<String> cssClasses = new ArrayList<>();
    private boolean caughtUp = false;
    private Boolean visible = null;

    /**
     * Create the object on a panel p. The preferred way of creating an object is using the
     * Panel::add(Control) method, instead of this
     * @param p
     */
    void create(AbstractDwcjPanel p) {};

    @Override
    public String getText() {
        if (ctrl != null) try {
            return ctrl.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return text;
    }

    @Override
    public IControl setText(String text) {
        if (ctrl != null) try {
            ctrl.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        if (text != null)
            this.text = new String(text.getBytes());
        else
            this.text = "<null>";
        return this;
    }

    /**
     * set the value for an attribute in the control
     * @param attribute the name of the attribute
     * @param value the value to be set
     * @return the control itself
     */
    public IControl setAttribute(String attribute, String value) {
        if (ctrl != null) try {
            ctrl.setAttribute(attribute, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        attributes.put(attribute, value);
        return this;
    }

    /**
     * get an attribute of the control
     * @param attribute the name of the attribute
     * @return the value of the attribute in the control
     */
    public String getAttribute(String attribute) {
        //ask the control first
        if (ctrl != null) try {
            return ctrl.getAttribute(attribute);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        //fall back to the internal list
        return attributes.get(attribute);
    }

    /*
    ------------------------------------------------------------
    internal protected methods that implement internal behaviour
    ------------------------------------------------------------
     */

    protected void setControlExpanse(IExpansible.Expanse expanse) {
        if (ctrl != null) try {
            switch (expanse) {
                case LARGE:
                    ctrl.setAttribute("expanse", "l");
                    break;
                case MEDIUM:
                    ctrl.setAttribute("expanse", "m");
                    break;
                case SMALL:
                    ctrl.setAttribute("expanse", "s");
                    break;
                case XLARGE:
                    ctrl.setAttribute("expanse", "xl");
                    break;
                case XSMALL:
                    ctrl.setAttribute("expanse", "xs");
                    break;
                default:
                    //noop
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.expanse = expanse;
    }


    protected void setControlTheme(IThemable.Theme theme) {
        if (ctrl != null) try {
            switch (theme) {
                case DEFAULT:
                    ctrl.setAttribute("theme", "default");
                    break;
                case DANGER:
                    ctrl.setAttribute("theme", "danger");
                    break;
                case GRAY:
                    ctrl.setAttribute("theme", "gray");
                    break;
                case INFO:
                    ctrl.setAttribute("theme", "info");
                    break;
                case PRIMARY:
                    ctrl.setAttribute("theme", "primary");
                    break;
                case SUCCESS:
                    ctrl.setAttribute("theme", "success");
                    break;
                case WARNING:
                    ctrl.setAttribute("theme", "warning");
                    break;
                default:
                    //noop
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.theme = theme;
    }


    protected void setControlStyle(String property, String value) {
        if (ctrl != null) try {
            ctrl.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.styles.put(property, value);
    }


    protected void addControlCssClass(String selector) {
        if (ctrl != null) try {
            ctrl.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.add(selector);
    }


    protected void removeControlCssClass(String selector) {
        if (ctrl != null) try {
            ctrl.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.remove(selector);
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

    public void setVisible(boolean b) {
        if (this.ctrl != null) try {
            ctrl.setVisible(b);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.visible = b;
    }

    /**
     * The catchUp method is used to replay attributes and settings that the API user might have
     * added to a control before its creation. A control is not created before it's added
     * to a panel. Anything that is added between instantiation of a control and its addition to a panel
     * has to be recorded and replayed in this method
     *
     * @throws IllegalAccessException - thrown if an attempt is made to call this method more than once
     */
    protected void catchUp() throws IllegalAccessException {
        if (caughtUp) throw new IllegalAccessException("catchUp cannot be called twice");
        if (!this.text.isEmpty()) {
            try {
                ctrl.setText(this.text);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }

        if (this.theme != null) {
            this.setControlTheme(this.theme);
        }

        if (this.expanse != null) {
            this.setControlExpanse(this.expanse);
        }

        if (!this.attributes.isEmpty()) {
            Iterator<String> it = this.attributes.keySet().iterator();
            while (it.hasNext()) {
                String key = it.next();
                this.setAttribute(key, this.attributes.get(key));
            }
        }

        if (!this.styles.isEmpty()) {
            Iterator<String> it = this.styles.keySet().iterator();
            while (it.hasNext()) {
                String key = it.next();
                this.setControlStyle(key, this.styles.get(key));
            }
        }

        if (!this.cssClasses.isEmpty()) {
            Iterator<String> it = this.cssClasses.iterator();
            while (it.hasNext()) {
                String cl = it.next();
                this.addControlCssClass(cl);
            }
        }

        if (this.visible != null) {
            this.setVisible(this.visible);
        }

        this.caughtUp = true;

    }
}
