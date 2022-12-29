package org.dwcj.controls.checkbox;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.checkbox.events.CheckBoxChangeEvent;
import org.dwcj.controls.checkbox.sinks.CheckBoxCheckEventSink;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.function.Consumer;

public final class CheckBox extends AbstractDwcControl implements HasReadOnly, Focusable, TabTraversable, TextAlignable {


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
    public enum HorizontalTextPosition{
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
    private ArrayList<Consumer<CheckBoxChangeEvent>> callbacks = new ArrayList<>();
    private CheckBoxCheckEventSink checkboxCheckEventSink;
    private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;
    private Boolean checked = false;


    /* =====================================================================================
     * Constructor initializes the inherited interface member variables to their defaults
     * =====================================================================================
     */
    public CheckBox(){
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT;
        
    }

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
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addCheckBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags);
            this.catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    /**
     * register an event callback for a checkOn or checkOff event
     *
     * @param callback A method to receive the onChange event
     * @return the control itself
     */
    public CheckBox onChange(Consumer<CheckBoxChangeEvent> callback) {
        if(this.ctrl != null){
            if(this.checkboxCheckEventSink == null){
                this.checkboxCheckEventSink = new CheckBoxCheckEventSink(this);
            }
            this.checkboxCheckEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
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
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setHorizontalTextPosition(position.position);
            } catch (BBjException e) {
                Environment.logError(e);
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
        if(this.ctrl != null){
            try {
                return ((BBjCheckBox) this.ctrl).isSelected();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    public CheckBox setChecked(Boolean checked) {
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setSelected(checked);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.checked = checked;
        return this;
    }



    /*=====================================================================================
     * This section overrides the various base class abstract methods in the 
     * AbstractDwcjControl class. These need to be should for method chaining 
     * purposes (i.e. setExample().setExample2().setExample3() ).
     * =====================================================================================
     */

    
    @Override
    public CheckBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public CheckBox setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public CheckBox setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public CheckBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public CheckBox setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public CheckBox setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public CheckBox setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public CheckBox addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public CheckBox removeClassName(String selector) {
        super.removeClassName(selector);
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
        if(this.ctrl != null){
            try{
                return ((BBjCheckBox) ctrl).isEditable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.readOnly;
    }

    /**
     * this method sets whether the CheckBox can be edited. True is editable, false is uneditable.
     * 
     * @param editable if true the control is editable
     * @return this
     */
    @Override
    public CheckBox setReadOnly(Boolean editable) {
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setEditable(editable);
            } catch (BBjException e) {
                Environment.logError(e);
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
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override
    public CheckBox setFocusable(Boolean focusable) {
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setFocusable(focusable);
            } catch (BBjException e) {
                Environment.logError(e);
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
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override
    public CheckBox setTabTraversable(Boolean traversable) {
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setTabTraversable(traversable);
            } catch (BBjException e) {
                Environment.logError(e);
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
        if(this.ctrl != null){
            try {
                ((BBjCheckBox) this.ctrl).setAlignment(alignment.textPosition);
            } catch (BBjException e) {
                Environment.logError(e);
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
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();
        
        if(this.checked != null){
            this.setChecked(this.checked);
        } 
        
        if(!this.callbacks.isEmpty()){
            this.checkboxCheckEventSink = new CheckBoxCheckEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.checkboxCheckEventSink.addCallback(this.callbacks.remove(0));
            }
        }

        
        if(this.horizontalTextPosition != HorizontalTextPosition.RIGHT){
            try{
                ((BBjCheckBox) ctrl).setHorizontalTextPosition(horizontalTextPosition.position);
            } catch(BBjException e){
                Environment.logError(e);
            }
            this.setHorizontalTextPosition(this.horizontalTextPosition);
        }

        if(Boolean.TRUE.equals(this.readOnly)){
            this.setReadOnly(true);
        }

        if(Boolean.FALSE.equals(this.focusable)){
            this.setFocusable(this.focusable);
        }

        if(Boolean.FALSE.equals(this.tabTraversable)){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.LEFT){
            this.setTextAlignment(this.textAlignment);
        }
    }
    
}
