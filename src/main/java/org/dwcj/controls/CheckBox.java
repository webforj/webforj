package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.CheckBoxChangeEvent;
import org.dwcj.events.sinks.CheckBoxCheckEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class CheckBox extends AbstractDwcControl implements IReadOnly, IFocusable, ITabTraversable, ITextAlignable {


    /*=====================================================================================
     * Initialize the enums for Expanse and Theme if applicable to the control.
     *=====================================================================================
     */

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    

    /*=====================================================================================
     * If a control has BBj integer constants, create an enum with parameterized constructors
     * that correspond to these numeric constants in BBj.
     * =====================================================================================
     */
    public static enum HorizontalTextPosition{
        RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);
        
        public final Integer position;
        
        private HorizontalTextPosition(Integer position){
            this.position = position;
        }
    }
    
    
    /* =====================================================================================
     * Create a member variable of the BBj component, casted from this.ctrl.
     * Initialize any other control-specific events or member variables as needed.
     * These extra member variables should be listed in the BBj documentation for each
     * control.
     * =====================================================================================
     */
    private Consumer<CheckBoxChangeEvent> callback = null;
    HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;
    Boolean checked = null;




    /*=====================================================================================
     *  This first section implements parameterized constructors, overrides the
     * create() method, and implements methods for the control-specific behaviors,
     * which often include getters and setters for control-specific member variables
     * and/or functionality.
     * =====================================================================================
     */

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
        if(this.ctrl != null){
            return this.horizontalTextPosition;
        }
        return HorizontalTextPosition.RIGHT;
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
        this.horizontalTextPosition = position;
        return this;
    }


    /**
     * Returns whether the BBjCheckBox is checked on or off (false = not checked, true = checked).
     * 
     * @return false if not checked, true if checked.
     */
    public Boolean isChecked() {
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



    /*=====================================================================================
     * This section overrides the various base class abstract methods in the 
     * AbstractDwcjControl class. These need to be overridden for method chaining 
     * purposes (i.e. setExample().setExample2().setExample3() ).
     * =====================================================================================
     */

    @Override
    public CheckBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public CheckBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public CheckBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public CheckBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public CheckBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public CheckBox setID(String id){
        super.setControlID(id);
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




    /*=====================================================================================
    * If Themes or Expanses are applicable for this control (if they have had Enums
    * implemented for their respective options), create the methods to set these by calling
    * the super method and returning this for chaining.
    * =====================================================================================
    */

    public CheckBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }




    /*=====================================================================================
     * Ensure that any interfaces which are applicable to the control have their methods
     * overridden.
     * =====================================================================================
     */

    /**
     * Returns whether the BBjCheckBox is editable (false = not editable, true = editable).
     * 
     * @return false if not editable, true if editable.
     */
    @Override
    public Boolean isReadOnly() {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try{
                return ((BBjCheckBox) ctrl).isEditable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    /**
     * this method sets whether the CheckBox can be edited. True is editable, false is uneditable.
     * 
     * @param editable
     * @return this
     */
    @Override
    public CheckBox setReadOnly(Boolean editable) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setEditable(editable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.readOnly = editable;
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                return ((BBjCheckBox) ctrl).isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public CheckBox setFocusable(Boolean focusable) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setFocusable(focusable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.focusable = focusable;
        return this;
    }
    
    
    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                return ((BBjCheckBox) ctrl).isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public CheckBox setTabTraversable(Boolean traversable) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setTabTraversable(traversable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.tabTraversable = traversable;
        return this;
    }


    @Override
    public Alignment getTextAlignment(){
        return this.textAlignment;
    }

    @Override
    public CheckBox setTextAlignment(Alignment alignment) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setAlignment(alignment.textPosition);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.textAlignment = alignment;
        return this;
    }




    /*=====================================================================================
     * Finally, override the catchUp() method - this is done by calling the super method,
     * and then catching up any control-specific member variables and/or interface 
     * variables for this control.
     * =====================================================================================
     */


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (this.caughtUp) throw new IllegalAccessException("catchUp cannot be called twice");

        super.catchUp();
        
        if(this.checked != null){
            this.setChecked(this.checked);
        } 
        if(this.callback != null){
            this.onChange(this.callback);
        }
        
        if(this.horizontalTextPosition != HorizontalTextPosition.RIGHT){
            try{
                ((BBjCheckBox) ctrl).setHorizontalTextPosition(horizontalTextPosition.position);
            } catch(BBjException e){
                e.printStackTrace();
            }
            this.setHorizontalTextPosition(this.horizontalTextPosition);
        }

        if(this.readOnly != null){
            this.setReadOnly(this.readOnly);
        }

        if(this.focusable != null){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != null){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != null){
            this.setTextAlignment(this.textAlignment);
        }
    }
    
}
