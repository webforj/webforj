package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.panels.IPanel;

public abstract class AbstractDwcControl implements IThemable, IExpansible, IStyleable {


    protected final static BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
    protected final static BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
    protected final static BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

    protected BBjControl ctrl;

    public abstract void create(IPanel p);

    public String getText() {
        try {
            return ctrl.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }


    @Override
    public void setExpanse(Expanse expanse) {
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


    @Override
    public void setTheme(Theme theme) {
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


    @Override
    public void setStyle(String property, String value) {
        try {
            ctrl.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void addClass(String selector) {
        try {
            ctrl.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void removeClass(String selector) {
        try {
            ctrl.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }


}
