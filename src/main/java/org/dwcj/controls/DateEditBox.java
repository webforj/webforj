package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.startup.type.sysgui.BBjColor;

import java.io.IOException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class DateEditBox extends AbstractDwcControl implements IReadOnly{
    
    private BBjInputD bbjDateEditBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    public DateEditBox(){}

    public DateEditBox(String text) { setText(text); }
    
    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputD(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            bbjDateEditBox = (BBjInputD) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * This method pops up a calendar dialog attached to the DateEditBox control
     * 
     */
    public void calendar() {
        ((BBjInputD) this.ctrl).calendar();
    }

    /**
     * This method returns whether the DateEditBox control beeps on invalid input.
     * 
     * @returns Returns whether the control will beep on invalid input (false = No Beep, true = Beep).
     * 
     */
    public boolean isBeep(){
        try {
            return bbjDateEditBox.getBeep();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    /**
     * This method returns the caret position in the DateEditBox control.
     * 
     * @returns Returns the position of the caret in the BBjInputD control.
     */
    public int getCaretPosition(){
        try {
            return bbjDateEditBox.getCaretPosition();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return -1;
    }
    
    /*==Throws an IOException - not sure if I handled this properly== */
    public String getEditString() throws IOException{
        try {
            return new String(bbjDateEditBox.getEditString(), "UTF_8");
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /**
     * This method returns the number of the last error generated in the DateEditBox control.
     * 
     * @returns Returns the position of the caret in the BBjInputD control.
     */
    public int getError(){
        try {
            return bbjDateEditBox.getError();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return -1;
    }
    
    public boolean isHighlighted(){
        try {
            return bbjDateEditBox.getHighlight();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }
    
    public boolean isInsertMode(){
        try {
            return bbjDateEditBox.getInsertMode();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    public int getLength(){
        try {
            return bbjDateEditBox.getLength();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return -1;
    }
    
    public String getLocale(){
            return bbjDateEditBox.getLocale();
    }

    public int getMargin(){
        try {
            return bbjDateEditBox.getMargin();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return -1;
    }

    public String getMask(){
        try {
            return bbjDateEditBox.getMask();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    public boolean isPassEnter(){
        try {
            return bbjDateEditBox.getPassEnter();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    public boolean isPassTab(){
        try {
            return bbjDateEditBox.getPassTab();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    public boolean isPlusMinus(){
        try {
            return bbjDateEditBox.getPlusMinus();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    public String getRestore(){
        try {
            return bbjDateEditBox.getRestore();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    public boolean isShowWeeks(){
        try {
            return bbjDateEditBox.getShowWeeks();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    /*==Unsure if this is the correct return type== */
    public Object getTodayColor(){
        try {
            return bbjDateEditBox.getTodayColor();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public Object getValue(){
        try {
            return bbjDateEditBox.getValue();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public Object getWeekdayColor(){
        try {
            return bbjDateEditBox.getWeekdayColor();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public Object getWeekendColor(){
        try {
            return bbjDateEditBox.getWeekendColor();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    @Override
    public boolean isReadOnly(){
        try {
            return bbjDateEditBox.isEditable();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return true;
    }

    public boolean isValid(){
        try {
            return bbjDateEditBox.isValid();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return false;
    }

    public void restore(){
        try {
            bbjDateEditBox.restore();
        } catch (BBjException e){
            e.printStackTrace();
        }
    }

    public void selectAll(){
        try {
            bbjDateEditBox.selectAll();
        } catch (BBjException e){
            e.printStackTrace();
        } 
    }

    public DateEditBox setBeep(boolean beep){
        try {
            bbjDateEditBox.setBeep(beep);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setCalendarSize(int width, int height){
        try {
            bbjDateEditBox.setCalendarSize(width, height);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    public DateEditBox setCaretPosition(int position){
        try {
            bbjDateEditBox.setCaretPosition(position);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    @Override
    public DateEditBox setReadOnly(boolean editable){
        try {
            bbjDateEditBox.setEditable(editable);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setEditString(String edit){
        try {
            bbjDateEditBox.setEditString(edit.getBytes());
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setHighlight(boolean highlight){
        try {
            bbjDateEditBox.setHighlight(highlight);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    public DateEditBox setInsertMode(boolean insert){
        try {
            bbjDateEditBox.setInsertMode(insert);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    public DateEditBox setLength(int length){
        try {
            bbjDateEditBox.setLength(length);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    public DateEditBox setLocale(String locale){
        bbjDateEditBox.setLocale(locale);
        return this;
    }

    public DateEditBox setMargin(int marginWidth){
        try {
            bbjDateEditBox.setMargin(marginWidth);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setMask(String mask){
        try {
            bbjDateEditBox.setMask(mask);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setPassEnter(boolean pass){
        try {
            bbjDateEditBox.setPassEnter(pass);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setPassTab(boolean pass){
        try {
            bbjDateEditBox.setPassTab(pass);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setRestore(String restore){
        try {
            bbjDateEditBox.setRestore(restore);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setPlusMinus(boolean plusMinus){
        try {
            bbjDateEditBox.setPlusMinus(plusMinus);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    
    public DateEditBox setShowWeeks(boolean showWeeks){
        try {
            bbjDateEditBox.setShowWeeks(showWeeks);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    /*==Unsure if simply casting this object to BBjColor is acceptable, likely
     * need to control/check input before passing?== -MH
     */
    public DateEditBox setTodayColor(Object color){
        try {
            bbjDateEditBox.setTodayColor((BBjColor)color);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    /*==Unsure if simply casting this object to BBjNumber is acceptable, likely
     * need to control/check input before passing?== -MH
     */
    public DateEditBox setValue(Object value){
        try{
            bbjDateEditBox.setValue((BBjNumber)value);
        } catch(BBjException e){
            e.printStackTrace();
        }
        return this;
    }

    /*==Unsure if simply casting this object to BBjColor is acceptable, likely
     * need to control/check input before passing?== -MH
     */
    public DateEditBox setWeekdayColor(Object color){
        try {
            bbjDateEditBox.setWeekdayColor((BBjColor)color);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }
    /*==Unsure if simply casting this object to BBjColor is acceptable, likely
     * need to control/check input before passing?== -MH
     */
    public DateEditBox setWeekendColor(Object color){
        try {
            bbjDateEditBox.setWeekendColor((BBjColor)color);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    public DateEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public DateEditBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public DateEditBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public DateEditBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    public DateEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    } 

    @Override
    public DateEditBox setID(String id){
        super.setID(id);
        return this;
    }
}
