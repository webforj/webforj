package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.CheckBoxChangeEvent;
import org.dwcj.events.sinks.CheckBoxCheckEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class CheckBox extends AbstractDwcControl implements IEditable {

    private Consumer<CheckBoxChangeEvent> callback;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCheckBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * == Still likely need to implement some of the other 
     * == methods outlined in the BBj documentation, but wanted 
     * == to check and see which of these methods were 
     * == going to be necessary before implementing them here.
     * -MH
     */

    /**
     * register an event callback for a checkOn or checkOff event
     *
     * @param callback A method to receive the onChange event
     * @return
     */
    public CheckBox onChange(Consumer<CheckBoxChangeEvent> callback) {
        this.callback = callback;
        new CheckBoxCheckEventSink(this, callback);
        return this;
    }

    /**
     * This method returns the horizontal position of the text in the CheckBox control. The default horizontal text position is RIGHT.
     *
     * @return This method returns the horizontal position of the text in the CheckBox control.
     */
    public int getHorizontalTextPosition(){
        try {
            return ((BBjCheckBox) this.ctrl).getHorizontalTextPosition();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether the BBjCheckBox is editable (false = not editable, true = editable).
     * 
     * @return false if not editable, true if editable.
     */
    @Override
    public boolean isEditable() {
        //todo: why could an exception be thrown?
        try {
            return ((BBjCheckBox) this.ctrl).isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether the BBjCheckBox is checked on or off (false = not checked, true = checked).
     * 
     * @return false if not checked, true if checked.
     */
    public boolean isSelected() {
        //todo: why could an exception be thrown?
        try {
            return ((BBjCheckBox) this.ctrl).isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * this method sets whether the CheckBox can be edited. True is editable, false is uneditable.
     * 
     * @param editable
     * @return this
     */
    @Override
    public CheckBox setEditable(boolean editable) {
        //todo: why could an exception be thrown?
        try {
            ((BBjCheckBox) this.ctrl).setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

  
    public CheckBox setHorizontalTextPosition(int position) {
        //todo: why could an exception be thrown?
        try {
            ((BBjCheckBox) this.ctrl).setHorizontalTextPosition(position);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public CheckBox setChecked(boolean selected) {
        //todo: why could an exception be thrown?
        try {
            ((BBjCheckBox) this.ctrl).setSelected(selected);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }



    public CheckBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public CheckBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public CheckBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public CheckBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public CheckBox setID(String id){
        super.setID(id);
        return this;
    }



}
