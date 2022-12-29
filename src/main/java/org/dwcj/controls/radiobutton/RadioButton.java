package org.dwcj.controls.radiobutton;

import com.basis.bbj.proxies.sysgui.BBjRadioButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.radiobutton.events.RadioButtonCheckEvent;
import org.dwcj.controls.radiobutton.sinks.RadioButtonCheckEventSink;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.function.Consumer;


public final class RadioButton extends AbstractDwcControl implements HasReadOnly, Focusable, TabTraversable {

    private BBjRadioButton bbjRadioButton;

    
    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public enum HorizontalTextPosition{
        RIGHT(4), LEFT(2), CENTER(0), LEADING(10), TRAILING(11);
        
        public final Integer position;
        
        private HorizontalTextPosition(Integer position){
            this.position = position;
        }
    }
    
    private ArrayList<Consumer<RadioButtonCheckEvent>> callbacks = new ArrayList<> ();
    private RadioButtonCheckEventSink checkEventSink; 
    private Boolean selected = false;
    private HorizontalTextPosition horizontalTextPosition = HorizontalTextPosition.RIGHT;

    public RadioButton(){
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addRadioButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags);
            bbjRadioButton = (BBjRadioButton) ctrl;
            catchUp();
        } catch (Exception e)  {
            Environment.logError(e);
        }
    }

    /**
     * register an event callback for a checkOn or checkOff event
     *
     * @param callback A method to receive the onCheck event
     * @return the control itself
     */
    public RadioButton onChange(Consumer<RadioButtonCheckEvent> callback) {
        if(this.ctrl != null){
            if(this.checkEventSink == null){
                this.checkEventSink = new RadioButtonCheckEventSink(this);
            }
            this.checkEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    public Integer getButtonID() {
        if(this.ctrl != null){
            try {
                return bbjRadioButton.getID();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        App.consoleError("ID cannot be fetched as control does not yet exist. Please add control to a window first");
        return null;
    }




    public HorizontalTextPosition getHorizontalTextPosition(){
        if(this.ctrl != null){
            return this.horizontalTextPosition;
        }
        return HorizontalTextPosition.RIGHT;
    }


    public RadioButton setHorizontalTextPosition(HorizontalTextPosition position) {
        if(this.ctrl != null){
            try {
                bbjRadioButton.setHorizontalTextPosition(position.position);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.horizontalTextPosition = position;
        return this;
    }


    public Boolean isSelected() {
        if(this.ctrl != null){
            try {
                return bbjRadioButton.isSelected();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }


    public RadioButton setSelected(boolean selected) {
        if(this.ctrl != null){
            try {
                bbjRadioButton.setSelected(selected);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }




    @Override
    public Boolean isReadOnly() {
        if(this.ctrl != null){
            try {
                return bbjRadioButton.isEditable();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.readOnly;
    }

    @Override
    public RadioButton setReadOnly(Boolean editable) {
        if(this.ctrl != null){
            try {
                bbjRadioButton.setEditable(editable);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjRadioButton.isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override
    public RadioButton setFocusable(Boolean focusable){
        if(this.ctrl != null) {
            try{
                bbjRadioButton.setFocusable(focusable);
            } catch (BBjException e){
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
                bbjRadioButton.isTabTraversable();
            } catch (BBjException e){
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override
    public RadioButton setTabTraversable(Boolean traverse){
        if(this.ctrl != null){
            try{
                bbjRadioButton.setTabTraversable(traverse);
            } catch (BBjException e){
                Environment.logError(e);
            }
        }
        this.tabTraversable = traverse;
        return this;
    }




    @Override
    public RadioButton setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public RadioButton setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public RadioButton setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public RadioButton setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public RadioButton setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public RadioButton setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public RadioButton setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public RadioButton addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public RadioButton removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }





    public RadioButton setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();
        
        if(Boolean.TRUE.equals(this.selected)){
            this.setSelected(this.selected);
        } 

        if(!this.callbacks.isEmpty()){
            this.checkEventSink = new RadioButtonCheckEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.checkEventSink.addCallback(this.callbacks.remove(0));
            }
        }
        
        if(this.horizontalTextPosition != HorizontalTextPosition.RIGHT){
            try{
                bbjRadioButton.setHorizontalTextPosition(horizontalTextPosition.position);
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

    }

}
