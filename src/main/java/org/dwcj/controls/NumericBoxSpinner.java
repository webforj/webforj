package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

//public final class NumericBoxSpinner extends NumericBox implements IThemable, IExpansible {
public final class NumericBoxSpinner extends NumericBox {

    private BBjInputNSpinner numBoxS;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputNSpinner(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            numBoxS = (BBjInputNSpinner) this.ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Number getMaximum(){
        try{
            numBoxS.getMaximum();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return null;
    }

    public NumericBoxSpinner setMaximum(Number num){
        try{
            numBoxS.setMaximum((BBjNumber)num);
        } catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public NumericBoxSpinner setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public NumericBoxSpinner setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public NumericBoxSpinner setID(String id){
        super.setID(id);
        return this;
    }
}