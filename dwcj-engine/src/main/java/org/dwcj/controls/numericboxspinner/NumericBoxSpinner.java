package org.dwcj.controls.numericboxspinner;

import com.basis.bbj.proxies.sysgui.BBjInputNSpinner;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.numericbox.NumericBox;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.HasMouseWheelCondition;
import org.dwcj.util.BBjFunctionalityHelper;

public final class NumericBoxSpinner extends NumericBox implements HasMouseWheelCondition {

    private BBjInputNSpinner numBoxS;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public NumericBoxSpinner(){
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addInputNSpinner(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, flags);
            numBoxS = (BBjInputNSpinner) this.numBox;
            this.numBox = (BBjInputNSpinner) this.ctrl;
            super.catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }
    

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
                Environment.logError(e);
            }
        }
        return this;
    }
    

    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        if(this.mouseWheelCondition != MouseWheelCondition.DEFAULT){
            this.setScrollWheelBehavior(this.mouseWheelCondition);
        }

    }

}