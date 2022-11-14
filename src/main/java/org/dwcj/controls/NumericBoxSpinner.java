package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

//public final class NumericBoxSpinner extends NumericBox implements IThemable, IExpansible {
public final class NumericBoxSpinner extends NumericBox implements IMouseWheelEnableable {

    private BBjInputNSpinner numBoxS;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public NumericBoxSpinner(){
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputNSpinner(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            numBoxS = (BBjInputNSpinner) this.numBox;
            this.numBox = (BBjInputNSpinner) this.ctrl;
            super.catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // public Number getMaximum(){
    //     try{
    //         numBoxS.getMaximum();
    //     } catch(BBjException e){
    //         e.printStackTrace();
    //     }
    //     return null;
    // }

    // public NumericBoxSpinner setMaximum(Number num){
    //     if(this.ctrl != null){
    //         try{
    //             numBoxS.setMaximum((BBjNumber)num);
    //         } catch(BBjException e){
    //             e.printStackTrace();
    //         }
    //     }
    //     return this;
    // } 
    

    @Override
    public MouseWheelCondition getScrollWheelBehavior(){
        return this.mouseWheelCondition;
    }

    @Override
    public NumericBoxSpinner setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                numBoxS.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }
    
    
    public NumericBoxSpinner setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public NumericBoxSpinner setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }


    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();

        if(this.mouseWheelCondition != MouseWheelCondition.DEFAULT){
            this.setScrollWheelBehavior(this.mouseWheelCondition);
        }

    }

}