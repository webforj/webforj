package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputD;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.startup.type.sysgui.BBjColor;

import java.io.IOException;

import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class DateEditBox extends AbstractDwcControl implements IReadOnly, IFocusable, ITabTraversable, ITextAlignable {
    
    private BBjInputD bbjDateEditBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    private Boolean beep = false;
    private Integer cHeight = null;
    private Integer cWidth = null;
    private Integer caretPos = 1;
    private String editString = null;
    private Boolean highlight = false;
    private Boolean insert = false;
    private Integer length = 8;
    private String locale = "en_US";
    private Integer margin = 3;
    private String mask = "%Mz/%Dz/%Yz";
    private Boolean pEnter = false;
    private Boolean pTab = false;
    private String restore = "0";
    private Boolean plusMinus = false;
    private Boolean showWeeks = false;



    public DateEditBox(){
        this("");
    }

    public DateEditBox(String text) { 
        setText(text);
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT; 
    }
    
    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputD(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            bbjDateEditBox = (BBjInputD) ctrl;
            catchUp();
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
    public Boolean isBeep(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getBeep();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.beep;
    }

    /**
     * This method returns the caret position in the DateEditBox control.
     * 
     * @returns Returns the position of the caret in the BBjInputD control.
     */
    public Integer getCaretPosition(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getCaretPosition();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.caretPos;
    }
    
    /*==Throws an IOException - not sure if I handled this properly== */
    public String getEditString() throws IOException{
        if(this.ctrl != null){
            try {
                return new String(bbjDateEditBox.getEditString(), "UTF_8");
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.editString;
    }

    /**
     * This method returns the number of the last error generated in the DateEditBox control.
     * 
     * @returns Returns the position of the caret in the BBjInputD control.
     */
    public Integer getError(){
        try {
            return bbjDateEditBox.getError();
        } catch (BBjException e){
            e.printStackTrace();
        } 
        return null;
    }
    
    public Boolean isHighlighted(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getHighlight();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return highlight;
    }
    
    public Boolean isInsertMode(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getInsertMode();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.insert;
    }

    public Integer getLength(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getLength();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.length;
    }
    
    public String getLocale(){
        if(this.ctrl != null){
            return bbjDateEditBox.getLocale();
        }
        return this.locale;
    }

    public Integer getMargin(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getMargin();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.margin;
    }

    public String getMask(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getMask();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.mask;
    }

    public Boolean isPassEnter(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getPassEnter();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.pEnter;
    }

    public Boolean isPassTab(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getPassTab();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.pTab;
    }

    public Boolean isPlusMinus(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getPlusMinus();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.plusMinus;
    }

    public String getRestore(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getRestore();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.restore;
    }

    public Boolean isShowWeeks(){
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.getShowWeeks();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this.showWeeks;
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
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.isValid();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return null;
    }

    public DateEditBox restore(){
        if(this.ctrl != null){
            try {
                bbjDateEditBox.restore();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }

    public DateEditBox selectAll(){
        if(this.ctrl != null){
            try {
                bbjDateEditBox.selectAll();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return this;
    }








    public DateEditBox setBeep(Boolean beep){
        App.consoleLog("In Beep");
        this.beep = beep;
        if(this.ctrl != null) {
            try {
                App.consoleLog(this.beep.toString());
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
        if(this.ctrl != null){
            try {
                return bbjDateEditBox.isEditable();
            } catch (BBjException e){
                e.printStackTrace();
            } 
        }
        return null;
    }

    @Override
    public DateEditBox setReadOnly(Boolean editable){
        if(this.ctrl != null){
            try {
                bbjDateEditBox.setEditable(editable);
            } catch (BBjException e){
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
                bbjDateEditBox.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public DateEditBox setFocusable(Boolean focusable){
        if(this.ctrl != null) {
            try{
                bbjDateEditBox.setFocusable(focusable);
            } catch (BBjException e){
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
                bbjDateEditBox.isTabTraversable();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return true;
    }

    @Override
    public DateEditBox setTabTraversable(Boolean traverse){
        if(this.ctrl != null){
            try{
                bbjDateEditBox.setTabTraversable(traverse);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traverse;
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        if(this.ctrl != null){
            return this.textAlignment;
        }
        return null;
    }

    @Override 
    public DateEditBox setTextAlignment(Alignment alignment){
        if(this.ctrl != null){
            try{
                bbjDateEditBox.setAlignment(alignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.textAlignment = alignment;
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

        App.consoleLog("In catchup" + this.beep.toString());

        if(this.beep != false){
            this.setBeep(this.beep);
        }

        if(this.caretPos != 1){
            this.setCaretPosition(this.caretPos);
        }

        if(this.cHeight != null && this.cWidth != null){
            this.setCalendarSize(this.cHeight, this.cWidth);
        }

        if(this.editString != null){
            this.setEditString(this.editString);
        }

        if(this.highlight != false){
            this.setHighlight(this.highlight);
        }

        if(this.insert != false){
            this.setInsertMode(this.insert);
        }

        if(this.length != 8){
            this.setLength(this.length);
        }

        if(this.locale != "en_US"){
            this.setLocale(this.locale);
        }

        if(this.margin != 3){
            this.setMargin(this.margin);
        }

        if(this.mask != "%Mz/%Dz/%Yz"){
            this.setMask(this.mask);
        }

        if(this.pEnter != false){
            this.setPassEnter(this.pEnter);
        }

        if(this.pTab != false){
            this.setPassTab(this.pTab);
        }

        if(this.restore != "0"){
            this.setRestore(this.restore);
        }

        if(this.plusMinus != false){
            this.setPlusMinus(this.plusMinus);
        }

        if(this.showWeeks != false){
            this.setShowWeeks(this.showWeeks);
        }




        if(this.readOnly != false){
            this.setReadOnly(this.readOnly);
        }

        if(this.focusable != null){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != true){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.LEFT){
            this.setTextAlignment(this.textAlignment);
        }

    }
}
