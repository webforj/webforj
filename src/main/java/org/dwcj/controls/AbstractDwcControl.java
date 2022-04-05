package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public abstract class AbstractDwcControl {


    protected final static BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
    protected final static BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
    protected final static BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

    protected BBjControl ctrl;

    static {
        ControlAccessor.setDefault(new CtrlAccessorImpl());
    }

    public abstract void create(AbstractDwcjPanel p);

    public String getText() {
        try {
            return ctrl.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    protected void setControlExpanse(IExpansible.Expanse expanse) {
        try {
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
    }

    public void setAttribute(String attribute, String value){
        try {
            ctrl.setAttribute(attribute, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public String getAttribute(String attribute){
        try {
            return ctrl.getAttribute(attribute);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    protected void setControlTheme(IThemable.Theme theme) {
        try {
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
    }


    protected void setControlStyle(String property, String value) {
        try {
            ctrl.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


    protected void addControlCssClass(String selector) {
        try {
            ctrl.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


    protected void removeControlCssClass(String selector) {
        try {
            ctrl.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


    BBjControl getControl() {
        return this.ctrl;
    }

    public void setVisible(boolean b) {
        try {
            ctrl.setVisible(b);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }
}
