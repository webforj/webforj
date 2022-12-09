package org.dwcj.controls.htmledit;

import com.basis.bbj.proxies.sysgui.BBjHtmlEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.interfaces.IFocusable;
import org.dwcj.interfaces.ITabTraversable;

import java.util.ArrayList;
import java.util.List;

public final  class HtmlEdit extends AbstractDwcControl implements IFocusable, ITabTraversable {

    private BBjHtmlEdit bbjHtmlEdit;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }


    

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte bFlag = (byte)0x00;

            if(!this.isEnabled()){
                bFlag += (byte)0x01;
            }
            if(!this.isVisible()){
                bFlag += (byte)0x10;
            }
            byte[] flags = new byte[]{(byte)0x00, bFlag};  

            ctrl = w.addHtmlEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "", flags);
            bbjHtmlEdit = (BBjHtmlEdit) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * This method returns a List of strings that specifies all available styles in the HtmlEdit toolbar.
     * @return List of strings that specifies all available styles in the HtmlEdit toolbar.
     */
    public List<String> getAllToolbarStyles() {
        try {
            return bbjHtmlEdit.getAllToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns the list of available spell-check languages for a HtmlEdit control.
     *
     * @return Returns a List of strings of available spell-check languages for a HtmlEdit control.
     */
    public List<String> getAvailableSpellCheckLanguages() {
        try {
            return bbjHtmlEdit.getAvailableSpellCheckLanguages();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns a List of strings that specifies all available editor states in the HTMLEditor toolbar.
     * 
     * @return Returns a list of strings that specifies all available editor stattes in the HTMLEditor toolbar.
     */
    public List<String> getAvailableStates() {
        try {
            return bbjHtmlEdit.getAvailableStates();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }


    /**
     * This method returns a boolean that indicates whether this HtmlEdit control is currently set to use a basic toolbar.
     * 
     * @return Returns a boolean that indicates whether this HtmlEdit control is currently set to use a basic toolbar .
     */
    public boolean isBasicToolbar() {
        try {
            return bbjHtmlEdit.getBasicToolbar();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method returns a List of strings that specifies the styles to be included when the HtmlEdit basic toolbar is selected.
     * 
     * @return Returns a List of strings that specifies the styles to be included when the HtmlEdit basic toolbar is selected.
     */
    public List<String> getBasicToolbarStyles() {
        try {
            return bbjHtmlEdit.getBasicToolbarStyles();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns a string representing the client type for this HTMLEdit control.
     * 
     * @return Returns a string representing the client type for the HtmlEdit control ("Browser" (BUI or DWC), "Swing" (basic HTML 3.2), "JavaFX" (WebKit), "Chromium").
     */
    public String getClientType() {
        try {
            return bbjHtmlEdit.getClientType();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the client version for this BBjHtmlEdit control.
     * @return Returns the version of the HtmlEdit control - the client versions will change over time. 
     */
    public String getClientVersion() {
        try {
            return bbjHtmlEdit.getClientVersion();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the UI locale of an HTMLEdit control.
     * @return A string representing the UI locale of the HTMLEdit control.
     */
    public String getLocale() {
        return bbjHtmlEdit.getLocale();
    }

    /**
     *  This method returns the list of available UI Locales for an HTMLEdit control.
     * @return a List of strings representing the available UI locales for an HTMLEdit control.
     */
    public List<String> getLocales() {
        try {
            return bbjHtmlEdit.getLocales();
        } catch (BBjException e) {
            e.printStackTrace();
            return new ArrayList<>();
        }
    }

    /**
     * This method returns the plain text content of an HTMLEditor control, without any HTML markup.
     * @return A string representing only the plaintext content of the HTML control.
     */
    public String getPlainText() {
        try {
            return bbjHtmlEdit.getPlainText();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the spell-check language of an HTMLEdit control.
     * @return A string representing the spell-check language of an HTMLEdit control.
     */
    public String getSpellCheckLanguage() {
        try {
            return bbjHtmlEdit.getSpellCheckLanguage();
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * This method returns the Boolean value of a specified state in the HtmlEdit toolbar.
     * @param String specifying one of the state names from HTMLEdit::getAvailableStates
     * @return Boolean value of the specified state in the HtmlEdit toolbar.
     */
    public boolean getState(String state) {
        try {
            return bbjHtmlEdit.getState(state);
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method returns a boolean that indicates whether spell-checking is currently enabled on this HTMLEdit control.
     * @return a boolean representing whether or not spell-check is enabled on this control.
     */
    public boolean isSpellChecked() {
        try {
            return bbjHtmlEdit.isSpellChecked();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * This method specifies whether this HTMLEdit control should use a basic toolbar
     * @param basicToolbar - Boolean representing whether or not to use a basic toolbar, true for yes, false for no
     * @return Returns this
     */
    public HtmlEdit setBasicToolbar(boolean basicToolbar) {
        try {
            bbjHtmlEdit.setBasicToolbar(basicToolbar);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method specifies the styles to be included when the HTMLEdit basic toolbar is used
     * @param styles - A List of styles to be shown when the basic toolbar is selected, should be an array of Strings
     * @return Returns this
     */
    public HtmlEdit setBasicToolbarStyles(ArrayList<String> styles) {
        try {
            bbjHtmlEdit.setBasicToolbarStyles((BBjVector) styles);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the UI locale of the HTMLEdit control
     * @param locale - A String representing the locale you wish to set the HTMLEdit to.
     * @return Returns this
     */
    public HtmlEdit setLocale(String locale) {
        bbjHtmlEdit.setLocale(locale);
        return this;
    }

    /**
     * Sets the text of an HtmlEdit control
     * @param locale - A String representing the text you wish to set.
     * @return Returns this
     */
    public HtmlEdit setPlainText(String text) {
        try {
            bbjHtmlEdit.setPlainText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets a specified state in the HtmlEdit toolbar to the specified boolean value
     * @param state - One of the state names from HtmlEdit::getAvailableStates()
     * @param value - Boolean value - true activates the specified state while false deactivates it. The null() method toggles it. Empty parameter is assumed null()
     * @return Returns this
     */
    public HtmlEdit setState(String state, boolean value) {
        try {
            bbjHtmlEdit.setState(state, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Specifies whether or not spell checking should be enabled in this HtmlEdit control
     * @param spellChecked - boolean value - true enables spell checking, false disables it.
     * @return Returns this
     */
    public HtmlEdit setSpellChecked(boolean spellChecked) {
        try {
            bbjHtmlEdit.setSpellChecked(spellChecked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the spell check language for this HtmlEdit control.
     * @param langeuage - String representing the desired language for spell checking
     * @return Returns this
     */
    public HtmlEdit setSpellCheckLanguage(String language) {
        try {
            bbjHtmlEdit.setSpellCheckLanguage(language);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }



    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                return bbjHtmlEdit.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.focusable;
    }

    @Override 
    public HtmlEdit setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjHtmlEdit.setFocusable(focusable);
            } catch(BBjException e){
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
                return bbjHtmlEdit.isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.tabTraversable;
    }

    @Override 
    public HtmlEdit setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjHtmlEdit.setTabTraversable(traversable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traversable;
        return this;
    }



    public HtmlEdit setText(String text) {
        super.setControlText(text);
        return this;
    }

    public HtmlEdit setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public HtmlEdit setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public HtmlEdit setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public HtmlEdit setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public HtmlEdit setID(String id){
        super.setControlID(id);
        return this;
    }

    public HtmlEdit setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public HtmlEdit addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public HtmlEdit removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    public HtmlEdit setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }



    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        //TODO
    }

}

