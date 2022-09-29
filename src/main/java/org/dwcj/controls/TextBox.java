package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;


public final class TextBox extends AbstractDwcControl implements IThemable {

    private BBjEditBox bbjEditBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public TextBox() {
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
            catchUp();
            bbjEditBox = (BBjEditBox) this.ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    public String getEditType(){
        return bbjEditBox.getEditType();
    }

    public int getMaxLength(){
        return bbjEditBox.getMaxLength();
    }

    public boolean isPassHomeDelete(){
        try{
            return bbjEditBox.getPassHomeDelete();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return false;
    }

    public String getSelectedText(){
        try{
            return bbjEditBox.getSelectedText();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return null;
    }

    /*Unsure if this is the correct return type for this functionality -MH */
    public Object[] getSelection(){
        try{
            return bbjEditBox.getSelection().toArray();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return null;
    }

    public boolean isEditable(){
        try{
            return bbjEditBox.isEditable();
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return true;
    }
    
    public boolean isPasswordVisible(){
        try{
            return bbjEditBox.isPasswordVisible();
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return false;
    }

    public TextBox select(int offset1, int offset2){
        bbjEditBox.select(offset1, offset2);
        return this;    
    }

    public TextBox setEditable(boolean editable){
        try{
            bbjEditBox.setEditable(editable);
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public TextBox setMaxLength(int length){
        try{
            bbjEditBox.setMaxLength(length);
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public TextBox setPassHomeDelete(boolean pass){
        try{
            bbjEditBox.setPassHomeDelete(pass);
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    public TextBox setPasswordVisible(boolean visible){
        try{
            bbjEditBox.setPasswordVisible(visible);
        }
        catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }


    public TextBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse.toString());
        return this;
    }

    @Override
    public TextBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public TextBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public TextBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public TextBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public TextBox setID(String id){
        super.setID(id);
        return this;
    }
}
