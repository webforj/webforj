package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;


public final class TextBox extends AbstractDwcControl implements IReadOnly, IFocusable, ITabTraversable, ITextAlignable, ITextControl {

    private BBjEditBox bbjEditBox;

    
    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }


    private Integer maxLength = 2147483647;
    private Boolean homeDelete = false;
    private Boolean passwordVisible = false;



    public TextBox() {
        this("");
    }

    public TextBox(String text) {
        setText(text);
    }

    
    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addEditBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, getText());
            bbjEditBox = (BBjEditBox) this.ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
        
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
                e.printStackTrace();
            }
        }
        return this.homeDelete;
    }

    public String getSelectedText(){
        if(this.ctrl != null){
            try{
                return bbjEditBox.getSelectedText();
            } catch(BBjException e){
                e.printStackTrace();
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
                e.printStackTrace();
            }
        }
        return null;
    }

    
    
    public boolean isPasswordVisible(){
        if(this.ctrl != null)
        try{
            return bbjEditBox.isPasswordVisible();
        }
        catch(BBjException e){
            e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
            }
        }
        this.passwordVisible = visible;
        return this;
    }





    @Override
    public Boolean isReadOnly(){
        try{
            return bbjEditBox.isEditable();
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return null;
    }
    
    @Override
    public TextBox setReadOnly(Boolean editable){
        try{
            bbjEditBox.setEditable(editable);
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjEditBox.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
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
                bbjEditBox.isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
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
    public TextBox setTextAlignment(Alignment alignment) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                bbjEditBox.setAlignment(alignment.textPosition);
            } catch (BBjException e) {
                e.printStackTrace();
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
                bbjEditBox.setHighlightOnFocus(highlight.highlight);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.textHighlight = highlight;
        return this;
    }




    public TextBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    public TextBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public TextBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public TextBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public TextBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public TextBox setID(String id){
        super.setControlID(id);
        return this;
    }

    public TextBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public TextBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public TextBox removeClass(String selector) {
        super.removeControlCssClass(selector);
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
        super.catchUp();

                
        if(this.maxLength != 2147483647){
            this.setMaxLength(this.maxLength);
        }

        if(this.homeDelete != false){
            this.setPassHomeDelete(this.homeDelete);
        }
        
        if(this.passwordVisible != false){
            this.setPasswordVisible(this.passwordVisible);
        }
        

        if(this.readOnly != false){
            this.setReadOnly(this.readOnly);
        }

        if(this.focusable != true){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != true){
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
