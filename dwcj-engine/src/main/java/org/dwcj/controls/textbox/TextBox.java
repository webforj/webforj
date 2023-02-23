package org.dwcj.controls.textbox;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import java.util.ArrayList;
import java.util.function.Consumer;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.textbox.events.TextBoxEditModifyEvent;
import org.dwcj.controls.textbox.sinks.TextBoxEditModifyEventSink;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.interfaces.TextHighlightable;
import org.dwcj.util.BBjFunctionalityHelper;


public final class TextBox extends AbstractDwcControl implements HasReadOnly, Focusable, TabTraversable, TextAlignable, TextHighlightable {

    private BBjEditBox bbjEditBox;

    
    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    private ArrayList<Consumer<TextBoxEditModifyEvent>> callbacks = new ArrayList<>();
    private TextBoxEditModifyEventSink editModifyEventSink;

    private Integer maxLength = 2147483647;
    private Boolean homeDelete = false;
    private Boolean passwordVisible = false;



    public TextBox() {
        this("");
    }

    public TextBox(String text) {
        setText(text);
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT;
        this.textHighlight = Highlight.HIGHLIGHT_NONE;
    }

    
    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText(), flags);
            bbjEditBox = (BBjEditBox) this.ctrl;
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
        
    }


    public TextBox onEditModify(Consumer<TextBoxEditModifyEvent> callback){
        if(this.ctrl != null){
            if(this.editModifyEventSink == null){
                this.editModifyEventSink = new TextBoxEditModifyEventSink(this);
            }
            this.editModifyEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    public String getEditType(){
        if(this.ctrl != null){
            return bbjEditBox.getEditType();
        }
        return "";
    }

    public Integer getMaxLength(){
        if(this.ctrl != null){
            return bbjEditBox.getMaxLength();
        }
        return this.maxLength;
    }

    public Boolean isPassHomeDelete(){
        if(this.ctrl != null){
            try{
                return bbjEditBox.getPassHomeDelete();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.homeDelete;
    }

    public String getSelectedText(){
        if(this.ctrl != null){
            try{
                return bbjEditBox.getSelectedText();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return null;
    }

    /*Unsure if this is the correct return type for this functionality -MH */

    /*Changed this to return a single string, otherwise could not get this to
     * properly work -MH
     */
    public String getSelection(){
        if(this.ctrl != null){
            try{
                return bbjEditBox.getSelection().toArray().toString();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return null;
    }

    
    
    public boolean isPasswordVisible(){
        if(this.ctrl != null){
            try{
                return bbjEditBox.isPasswordVisible();
            }
            catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.passwordVisible;
    }

    public TextBox select(Integer offset1, Integer offset2){
        if(this.ctrl != null){
            bbjEditBox.select(offset1, offset2);
        }
        return this;    
    }

    

    public TextBox setMaxLength(Integer length){
        if(this.ctrl != null){
            try{
                bbjEditBox.setMaxLength(length);
            }
            catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.maxLength = length;
        return this;
    }

    public TextBox setPassHomeDelete(Boolean pass){
        if(this.ctrl != null){
            try{
                bbjEditBox.setPassHomeDelete(pass);
            }
            catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.homeDelete = pass;
        return this;
    }

    public TextBox setPasswordVisible(Boolean visible){
        if(this.ctrl != null){
            try{
                bbjEditBox.setPasswordVisible(visible);
            }
            catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.passwordVisible = visible;
        return this;
    }





    @Override
    public Boolean isReadOnly(){
        try{
            return !bbjEditBox.isEditable();
        }
        catch(BBjException e){
            Environment.logError(e);
        }
        return this.readOnly;
    }
    
    @Override
    public TextBox setReadOnly(Boolean editable){
        try{
            bbjEditBox.setEditable(!editable);
        }
        catch(BBjException e){
            Environment.logError(e);
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjEditBox.isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override 
    public TextBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjEditBox.setFocusable(focusable);
            } catch(BBjException e){
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
                bbjEditBox.isTabTraversable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override
    public TextBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjEditBox.setTabTraversable(traversable);
            } catch(BBjException e){
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
    public TextBox setTextAlignment(Alignment alignment) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                bbjEditBox.setAlignment(alignment.textPosition);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.textAlignment = alignment;
        return this;
    }


    @Override
    public Highlight getHighlightOnFocus(){
        return this.textHighlight;
    } 

    @Override
    public TextBox setHighlightOnFocus(Highlight highlight){
        if(this.ctrl != null){
            try{
                bbjEditBox.setHighlightOnFocus(highlight.highlightType);
            } catch (BBjException e){
                Environment.logError(e);
            }
        }
        this.textHighlight = highlight;
        return this;
    }




    @Override
    public TextBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public TextBox setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public TextBox setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public TextBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TextBox setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public TextBox setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public TextBox setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public TextBox addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public TextBox removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }




    public TextBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public TextBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }


    @Override
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        if(!this.callbacks.isEmpty()){
            this.editModifyEventSink = new TextBoxEditModifyEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.editModifyEventSink.addCallback(this.callbacks.remove(0));
            }
        }
                
        if(this.maxLength != 2147483647){
            this.setMaxLength(this.maxLength);
        }

        if(Boolean.TRUE.equals(this.homeDelete)){
            this.setPassHomeDelete(this.homeDelete);
        }
        
        if(Boolean.TRUE.equals(this.passwordVisible)){
            this.setPasswordVisible(this.passwordVisible);
        }
        

        if(Boolean.TRUE.equals(this.readOnly)){
            this.setReadOnly(this.readOnly);
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

        if(this.textHighlight != Highlight.HIGHLIGHT_NONE){
            this.setHighlightOnFocus(this.textHighlight);
        }
    }



}
