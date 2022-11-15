package org.dwcj.controls;

import java.nio.charset.StandardCharsets;

import org.dwcj.App;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public final class StringEditBox extends AbstractDwcControl implements IReadOnly, IFocusable, ITabTraversable, ITextAlignable, ITextControl {

    
    private BBjInputE bbjInputE;
    
    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }
    
    
    private Integer caretPos = 1;
    private String editString = "";
    private Boolean highlight = false;
    private Boolean insert = false;
    private Integer length = null;
    private Integer margin = 7;
    private String mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
    private String pad = " ";
    private Boolean passEnter = false;
    private Boolean passTab = false;
    private String restore = "";
       
    
    
    public StringEditBox(){
        this("");
    }

    public StringEditBox(String text) {
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
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            bbjInputE = (BBjInputE) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    public Integer getCaretPos(){
        if(this.ctrl != null){
            try{
                return bbjInputE.getCaretPosition();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return this.caretPos;
    }

    public Integer getError() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getError();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public String getEditString() {
        if(this.ctrl != null){
            try {
                return new String(bbjInputE.getEditString(), StandardCharsets.UTF_8);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.editString;
    }

    public Boolean isHighlight() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getHighlight();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.highlight;
    }

    public Boolean isInsertMode() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getInsertMode();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.insert;
    }

    public Integer getLength() {
        if(this.ctrl != null){
            try {
                bbjInputE.getLength();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.length;
    }

    public Integer getMargin() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getMargin();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return -1;
    }

    public String getMask() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getMask();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.mask;
    }


    public String getPadCharacter() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPadCharacter();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.pad;
    }

    public Boolean isPassEnter() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPassEnter();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.passEnter;
    }

    public Boolean isPassTab() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPassTab();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.passTab;
    }

    public String getRestore() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getRestore();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.restore;
    }



    public StringEditBox restore() {
        if(this.ctrl != null){
            try {
                bbjInputE.restore();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    public StringEditBox selectAll() {
        if(this.ctrl != null){
            try {
                bbjInputE.selectAll();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }


    public StringEditBox setCaretPos(Integer position){
        if(this.ctrl != null){
            try{
                bbjInputE.setCaretPosition(position);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.caretPos = position;
        return this;
    }

    public StringEditBox setEditString(String edit) {
        if(this.ctrl != null){
            try {
                bbjInputE.setEditString(edit.getBytes(StandardCharsets.UTF_8));
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.editString = edit;
        return this;
    }

    public StringEditBox setHighlight(Boolean highlight) {
        if(this.ctrl != null){
            try {
                bbjInputE.setHighlight(highlight);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.highlight = highlight;
        return this;
    }

    public StringEditBox setInsertMode(Boolean insert) {
        if(this.ctrl != null){
            try {
                bbjInputE.setInsertMode(insert);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.insert = insert;
        return this;
    }

    public StringEditBox setLength(Integer len) {
        if(this.ctrl != null){
            try {
                bbjInputE.setLength(len);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.length = len;
        return this;
    }

    public StringEditBox setMargin(Integer marginWidth) {
        if(this.ctrl != null){
            try {
                bbjInputE.setMargin(marginWidth);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.margin = marginWidth;
        return this;
    }

    public StringEditBox setMask(String mask){
        if (ctrl != null){
        try {
            ((BBjInputE)ctrl).setMask(mask);
        } catch (BBjException e) {
            App.consoleLog(e.getMessage());
            throw new RuntimeException(e);
        }
        }
        this.mask = mask;
        return this;
    }

    public StringEditBox setPadCharacter(String pad) {
        if(this.ctrl != null){
            try {
                bbjInputE.setPadCharacter(pad);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.pad = pad;
        return this;
    }

    public StringEditBox setPassEnter(Boolean pass) {
        if(this.ctrl != null){
            try {
                bbjInputE.setPassEnter(pass);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.passEnter = pass;
        return this;
    }

    public StringEditBox setPassTab(Boolean pass) {
        if(this.ctrl != null){
            try {
                bbjInputE.setPassTab(pass);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.passTab = pass;
        return this;
    }

    public StringEditBox setRestore(String restore) {
        if(this.ctrl != null){
            try {
                bbjInputE.setRestore(restore);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.restore = restore;
        return this;
    }





    @Override
    public Boolean isReadOnly() {
        try {
            return bbjInputE.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }
    @Override
    public StringEditBox setReadOnly(Boolean editable) {
        try {
            bbjInputE.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjInputE.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.focusable;
    }

    @Override 
    public StringEditBox setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjInputE.setFocusable(focusable);
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
                bbjInputE.isTabTraversable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this.tabTraversable;
    }

    @Override
    public StringEditBox setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjInputE.setTabTraversable(traversable);
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
    public StringEditBox setTextAlignment(Alignment alignment) {
        //todo: why could an exception be thrown?
        if(this.ctrl != null){
            try {
                bbjInputE.setAlignment(alignment.textPosition);
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
    public StringEditBox setHighlightOnFocus(Highlight highlight){
        if(this.ctrl != null){
            try{
                bbjInputE.setHighlightOnFocus(highlight.highlight);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.textHighlight = highlight;
        return this;
    }



    public StringEditBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    public StringEditBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public StringEditBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public StringEditBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public StringEditBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public StringEditBox setID(String id){
        super.setControlID(id);
        return this;
    }

    public StringEditBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public StringEditBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public StringEditBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }





    public StringEditBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }


    public StringEditBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }






    @Override
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();
        
        if(this.caretPos != 1){
            this.setCaretPos(this.caretPos);
        }

        if(this.editString != ""){
            this.setEditString(this.editString);
        }

        if(this.highlight != false){
            this.setHighlight(this.highlight);
        }

        if(this.insert != false){
            this.setInsertMode(this.insert);
        }

        if(this.length != null){
            this.setLength(this.length);
        }

        if(this.margin != 7){
            this.setMargin(this.margin);
        }

        if(this.mask != "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"){
            this.setMask(this.mask);
        }

        if(this.pad != " "){
            this.setPadCharacter(this.pad);
        }

        if(this.passEnter != false){
            this.setPassEnter(this.passEnter);
        }

        if(this.passTab != false){
            this.setPassTab(this.passTab);
        }

        if(this.restore != ""){
            this.setRestore(this.restore);
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
