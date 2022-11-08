package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.startup.type.sysgui.BBjColor;

import java.io.IOException;

import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class DateEditBox extends AbstractDwcControl implements IReadOnly, IFocusable {
    
    private BBjInputD bbjDateEditBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    private Boolean beep = null;
    private Integer cHeight = null;
    private Integer cWidth = null;
    private Integer caretPos = null;
    private String editString = null;
    private Boolean highlight = null;
    private Boolean insert = null;
    private Integer length = null;
    private String locale = null;
    private Integer margin = null;
    private String mask = null;
    private Boolean pEnter = null;
    private Boolean pTab = null;
    private String restore = null;
    private Boolean plusMinus = null;
    private Boolean showWeeks = null;



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
    public String getTodayColor(){
        try {
            return bbjDateEditBox.getTodayColor().toString();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public String getValue(){
        try {
            return bbjDateEditBox.getValue().toString();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public String getWeekdayColor(){
        try {
            return bbjDateEditBox.getWeekdayColor().toString();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    /*==Unsure if this is the correct return type== */
    public String getWeekendColor(){
        try {
            return bbjDateEditBox.getWeekendColor().toString();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }




    


    public Boolean isValid(){
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

    public DateEditBox setBeep(Boolean beep){
        this.beep = beep;
        if(this.ctrl != null) {
            try {
                bbjDateEditBox.setBeep(beep);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setCalendarSize(int width, int height){
        this.cWidth = width;
        this.cHeight = height;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setCalendarSize(width, height);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }

    public DateEditBox setCaretPosition(int position){
        this.caretPos = position;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setCaretPosition(position);
            } catch (BBjException e){
                e.printStackTrace();
            }

        }
        return this; 
    }
    

    
    public DateEditBox setEditString(String edit){
        this.editString = edit;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setEditString(edit.getBytes());
            } catch (BBjException e){
                e.printStackTrace();
            }
        }

        return this; 
    }
    
    public DateEditBox setHighlight(Boolean highlight){
        this.highlight = highlight;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setHighlight(highlight);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }

        return this; 
    }


    public DateEditBox setInsertMode(Boolean insert){
        this.insert = insert;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setInsertMode(insert);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
    
        return this; 
    }

    public DateEditBox setLength(Integer length){
        this.length = length;
        if(ctrl != null){
            try {
                bbjDateEditBox.setLength(length);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }


    public DateEditBox setLocale(String locale){
        this.locale = locale;
        if(this.ctrl != null){
            bbjDateEditBox.setLocale(locale);
        }
        return this;
    }

    public DateEditBox setMargin(Integer marginWidth){
        this.margin = marginWidth;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setMargin(marginWidth);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setMask(String mask){
        this.mask = mask;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setMask(mask);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setPassEnter(Boolean pass){
        this.pEnter = pass;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setPassEnter(pass);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setPassTab(Boolean pass){
        this.pTab = pass;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setPassTab(pass);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setRestore(String restore){
        this.restore = restore;
        if(this.ctrl != null){
            try {
            bbjDateEditBox.setRestore(restore);
        } catch (BBjException e){
            e.printStackTrace();
        }
        }
        return this; 
    }
    
    public DateEditBox setPlusMinus(Boolean plusMinus){
        this.plusMinus = plusMinus;
        if(ctrl != null){
            try {
                bbjDateEditBox.setPlusMinus(plusMinus);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this; 
    }
    
    public DateEditBox setShowWeeks(boolean showWeeks){
        this.showWeeks = showWeeks;
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setShowWeeks(showWeeks);
            } catch (BBjException e){
                e.printStackTrace();
            }
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







    @Override
    public Boolean isReadOnly(){
        try {
            return bbjDateEditBox.isEditable();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }

    @Override
    public DateEditBox setReadOnly(Boolean editable){
        try {
            bbjDateEditBox.setEditable(editable);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this; 
    }

    @Override
    public Boolean isFocusable(){
        try{
            bbjDateEditBox.isFocusable();
        } catch(BBjException e){
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public DateEditBox setFocusable(Boolean focusable){
        try{
            bbjDateEditBox.setFocusable(focusable);
        } catch (BBjException e){
            e.printStackTrace();
        }
        return this;
    }






    @Override
    public DateEditBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public DateEditBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public DateEditBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public DateEditBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public DateEditBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public DateEditBox setID(String id){
        super.setControlID(id);
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




    public DateEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public DateEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    } 


    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();

        if(this.focusable != null){
            this.setFocusable(this.focusable);
        }

        if(this.beep != null){
            this.setBeep(this.beep);
        }

        if(this.caretPos != null){
            this.setCaretPosition(this.caretPos);
        }

        if(this.cHeight != null && this.cWidth != null){
            this.setCalendarSize(this.cHeight, this.cWidth);
        }

        if(this.editString != null){
            this.setEditString(this.editString);
        }

        if(this.highlight != null){
            this.setHighlight(this.highlight);
        }

        if(this.insert != null){
            this.setInsertMode(this.insert);
        }

        if(this.length != null){
            this.setLength(this.length);
        }

        if(this.locale != null){
            this.setLocale(this.locale);
        }

        if(this.margin != null){
            this.setMargin(this.margin);
        }

        if(this.mask!=null){
            this.setMask(this.mask);
        }

        if(this.pEnter != null){
            this.setPassEnter(this.pEnter);
        }

        if(this.pTab != null){
            this.setPassTab(this.pTab);
        }

        if(this.restore != null){
            this.setRestore(this.restore);
        }

        if(this.plusMinus != null){
            this.setPlusMinus(this.plusMinus);
        }

        if(this.showWeeks != null){
            this.setShowWeeks(this.showWeeks);
        }

        if(this.readOnly != null){
            this.setReadOnly(this.readOnly);
        }

        if(this.focusable != null){
            this.setFocusable(this.focusable);
        }

    }
}
