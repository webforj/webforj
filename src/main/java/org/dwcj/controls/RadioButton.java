package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import org.dwcj.events.RadioButtonCheckEvent;
import org.dwcj.events.sinks.RadioButtonCheckEventSink;

import java.util.function.Consumer;


public final class RadioButton extends AbstractDwcControl implements IEditable{

    private BBjRadioButton bbjRadioButton;

    private Consumer<RadioButtonCheckEvent> callback;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addRadioButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            bbjRadioButton = (BBjRadioButton) ctrl;
            catchUp();
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }

    /**
     * register an event callback for a checkOn or checkOff event
     *
     * @param callback A method to receive the onCheck event
     * @return
     */
    public RadioButton onCheck(Consumer<RadioButtonCheckEvent> callback) {
        this.callback = callback;
        new RadioButtonCheckEventSink(this, callback);
        return this;
    }

    public int getButtonID() {
        try {
            return bbjRadioButton.getID();
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public RadioButtonGroup getRadioButtonGroup() {
        try {
            BBjControl bbjGroup = (BBjControl) bbjRadioButton.getRadioGroup();
            int id = bbjGroup.getID();
            return RadioButtonGroup.getGroupByID(id);
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public boolean isEditable() {
        try {
            return bbjRadioButton.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean isSelected() {
        try {
            return bbjRadioButton.isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public RadioButton setEditable(boolean editable) {
        try {
            bbjRadioButton.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public void setSelected(boolean selected) {
        try {
            bbjRadioButton.setSelected(selected);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public RadioButton setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public RadioButton setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public RadioButton addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public RadioButton removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public RadioButton setID(String id){
        super.setID(id);
        return this;
    }
}
