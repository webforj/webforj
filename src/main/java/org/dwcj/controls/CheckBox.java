package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.CheckBoxChangeEvent;
import org.dwcj.events.sinks.CheckBoxCheckEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class CheckBox extends AbstractDwcControl implements IReadOnly, IExpansible {

    
    /*
    * These values represent the underlying integer constants in 
    */
    public static enum HorizontalTextPosition{
        RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);
        
        public final Integer position;
        
        private HorizontalTextPosition(Integer position){
            this.position = position;
        }
    }
    
    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    private Consumer<CheckBoxChangeEvent> callback = null;
    HorizontalTextPosition position = HorizontalTextPosition.RIGHT;
    Boolean checked = null;




    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCheckBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            this.catchUp();
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
        if(this.ctrl != null){
            new CheckBoxCheckEventSink(this, callback);
        }
        this.callback = callback;
        
        return this;
    }

    /**
     * This method returns the horizontal position of the text in the CheckBox control. The default horizontal text position is RIGHT.
     *
     * @return This method returns the horizontal position of the text in the CheckBox control.
     */
    public HorizontalTextPosition getHorizontalTextPosition(){
        return this.position;
    }

    /**
     * Returns whether the BBjCheckBox is editable (false = not editable, true = editable).
     * 
     * @return false if not editable, true if editable.
     */
    @Override
    public Boolean isReadOnly() {
        //todo: why could an exception be thrown?
            // return ((BBjCheckBox) this.ctrl).isEditable();
        return super.isReadOnly();
    }

    /**
     * Returns whether the BBjCheckBox is checked on or off (false = not checked, true = checked).
     * 
     * @return false if not checked, true if checked.
     */
    public boolean isChecked() {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                return ((BBjCheckBox) this.ctrl).isSelected();
            } catch (BBjException e) {
                e.printStackTrace();
            }
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
    public CheckBox setReadOnly(boolean editable) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setEditable(editable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        super.setReadOnly(editable);
        return this;
    }

  
    public CheckBox setHorizontalTextPosition(HorizontalTextPosition position) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setHorizontalTextPosition(position.position);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.position = position;
        return this;
    }

    public CheckBox setChecked(Boolean checked) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setSelected(checked);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.checked = checked;
        return this;
    }


    @Override
    public CheckBox setExpanse(Enum<?> expanse) {
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

    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (this.caughtUp) throw new IllegalAccessException("catchUp cannot be called twice");

        super.catchUp();
        
        if(this.position != null){
            this.setHorizontalTextPosition(this.position);
        }
        if(this.checked != null){
            this.setChecked(this.checked);
        } 
        if(this.callback != null){
            this.onChange(this.callback);
        }

        this.caughtUp = true;

    }


}
