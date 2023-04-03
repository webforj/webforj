package org.dwcj.component.button;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextAlignable;
import org.dwcj.component.button.event.ButtonClickEvent;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * A Push Button
 */
public final class Button extends AbstractDwcComponent implements Focusable,  TabTraversable, TextAlignable{



    /*=====================================================================================
     * Initialize the enums for Expanse and Theme if applicable to the control.
     *=====================================================================================
     */

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL;
    }
    
    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS, OUTLINED_PRIMARY,
        OUTLINED_WARNING
    }



    /*=====================================================================================
     * If a control has BBj integer constants, create an enum with parameterized constructors
     * that correspond to these numeric constants in BBj.
     * =====================================================================================
     */
    
    public enum TextVerticalAlignment{
        TOP(1), CENTER(0), BOTTOM(3);
        
        public final Integer alignment;
        
        private TextVerticalAlignment(Integer alignment){
            this.alignment = alignment;
        }
    }
    


    /* =====================================================================================
     * Create a member variable of the BBj component, casted from this.ctrl.
     * Initialize any other control-specific events or member variables as needed.
     * These extra member variables should be listed in the BBj documentation for each
     * control.
     * =====================================================================================
     */
    
    private ArrayList<Consumer<ButtonClickEvent>> callbacks = new ArrayList<>();
    private ButtonClickEventSink buttonClickEventSink;
    private Boolean disableOnClick = false;

    private Expanse expanse = null;
    private Theme theme = Theme.DEFAULT;
    private TextVerticalAlignment verticalAlignment = TextVerticalAlignment.CENTER;
    
    
    



    /*=====================================================================================
     *  This first section implements parameterized constructors, overrides the
     * create() method, and implements methods for the control-specific behaviors,
     * which often include getters and setters for control-specific member variables
     * and/or functionality. Constructors initialize the inherited interface member
     * variables.
     * =====================================================================================
     */

    public Button() {
        this("");
    }

    /**
     * Parameterized button constructor, accepts a string as an argument which will be the initial text displayed on the button
     * @param text String value for initial button text
     */
    public Button(String text) {
        super.setText(text);
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.MIDDLE;
    }

    @Override
    protected void create(AbstractWindow p) {

        try {
            BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText(), flags);
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */

    public Button onClick(Consumer<ButtonClickEvent> callback) {
        if(this.ctrl != null){
            if(this.buttonClickEventSink == null){
                this.buttonClickEventSink = new ButtonClickEventSink(this);
            }
            this.buttonClickEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    /**
     * Accessor for whether or not the button is disabled. 
     * @return Boolean value 
     */
    public Boolean isDisableOnClick() {
        if(this.ctrl != null){
            try {
                ((BBjButton) ctrl).getDisableOnClick();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.disableOnClick;
    }

    /**
     * Mutator for whether or not the button is disabled on click
     * @param disable Boolean value
     * @return Instance of the object to enable method chaining.
     */
    public Button setDisableOnClick(Boolean disable) {
        if(this.ctrl != null){
            try {
                ((BBjButton) ctrl).setDisableOnClick(disable);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.disableOnClick = disable;
        return this;
    }


    /**
     * Accessor for the vertical alignment of text within the button
     * @return Enum value of text's vertical alignment
     */
    public TextVerticalAlignment getVerticalAlignment(){
        if(this.ctrl != null){
            return this.verticalAlignment;
        }
        return this.verticalAlignment;
    }

    /**
     * Mutator for the vertical alignment of text within the button
     * @return The Button itself
     * @param Button TextVerticalAlignment enum value
     */
    public Button setVerticalAlignment(TextVerticalAlignment alignment){
        if(this.ctrl != null){
            try{
                ((BBjButton) ctrl).setVerticalAlignment(alignment.alignment);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.verticalAlignment = alignment;
        return this;
    }


    /*=====================================================================================
     * This section overrides the various base class abstract methods in the 
     * AbstractDwcjControl class. These need to be should for method chaining 
     * purposes (i.e. setExample().setExample2().setExample3() ).
     * =====================================================================================
     */

    @Override
    public Button setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Button setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public Button setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public Button setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public Button setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public Button setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public Button setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public Button addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public Button removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }




    /*=====================================================================================
    * If Themes or Expanses are applicable for this control (if they have had Enums
    * implemented for their respective options), create the methods to set these by calling
    * the super method and returning this for chaining.
    * =====================================================================================
    */

    /**
     * Accessor to return the button object's current expanse
     * @return Expanse enum from the button class
     */
    public Expanse getExpanse(){
        return this.expanse;
    }

    /**
     * Mutator to change the expanse of a button that requires a specific button enum value
     * @param expanse button expanse enum value
     * @return The button object itself
     */
    public Button setExpanse(Expanse expanse) {
            super.setControlExpanse(expanse);
            return this;
    }

    /**
     * Accessor to return the button object's current theme
     * @return Expanse enum from the button class
     */
    public Theme getTheme(){
        return this.theme;
    }

    /**
     * Mutator to change the theme of a button that requires a specific button enum value
     * @param theme button theme enum value
     * @return The button object itself
     */
    public Button setTheme(Theme theme) {
            super.setControlTheme(theme);
            return this;
    }




    /*=====================================================================================
    * Ensure that any interfaces which are applicable to the control have their methods
    * overridden.
    * =====================================================================================
    */

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                ((BBjButton) ctrl).isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override 
    public Button setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                ((BBjButton) ctrl).setFocusable(focusable);
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
                ((BBjButton) ctrl).isTabTraversable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override 
    public Button setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                ((BBjButton) ctrl).setTabTraversable(traversable);
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
    public Button setTextAlignment(Alignment alignment){
        if(this.ctrl != null){
            try{
                ((BBjButton) ctrl).setAlignment(alignment.textPosition);
            } catch(BBjException e){
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

        if(Boolean.TRUE.equals(this.disableOnClick)){
            this.setDisableOnClick(this.disableOnClick);
        }

        if(!this.callbacks.isEmpty()){
            this.buttonClickEventSink = new ButtonClickEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.buttonClickEventSink.addCallback(this.callbacks.remove(0));
            }
        }

        if(Boolean.FALSE.equals(this.focusable)){
            this.setFocusable(this.focusable);
        }

        if(Boolean.FALSE.equals(this.tabTraversable)){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.MIDDLE){
            this.setTextAlignment(this.textAlignment);
        }

        if(this.expanse != null){
            this.setExpanse(this.expanse);
        }

        if(this.theme != Theme.DEFAULT){
            this.setTheme(this.theme);
        }

    }
        
}
