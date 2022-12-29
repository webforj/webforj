package org.dwcj.controls.stringeditbox;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.function.Consumer;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.stringeditbox.events.StringEditBoxEditModifyEvent;
import org.dwcj.controls.stringeditbox.sinks.StringEditBoxEditModifyEventSink;
import org.dwcj.interfaces.Focusable;
import org.dwcj.interfaces.HasReadOnly;
import org.dwcj.interfaces.TabTraversable;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.interfaces.TextHighlightable;
import org.dwcj.util.BBjFunctionalityHelper;

import com.basis.bbj.proxies.sysgui.BBjInputE;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public final class StringEditBox extends AbstractDwcControl implements HasReadOnly, Focusable, TabTraversable, TextAlignable, TextHighlightable {

    
    private BBjInputE bbjInputE;
    
    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }
    

    private ArrayList<Consumer<StringEditBoxEditModifyEvent>> callbacks = new ArrayList<>();
    private StringEditBoxEditModifyEventSink editModifyEventSink;
    
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
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addInputE(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, flags);
            bbjInputE = (BBjInputE) ctrl;
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public StringEditBox onEditModify(Consumer<StringEditBoxEditModifyEvent> callback){

        if(this.ctrl != null){
            if(this.editModifyEventSink == null){
                this.editModifyEventSink = new StringEditBoxEditModifyEventSink(this);
            }
            this.editModifyEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }


    public Integer getCaretPos(){
        if(this.ctrl != null){
            try{
                return bbjInputE.getCaretPosition();
            } catch (BBjException e){
                Environment.logError(e);
            }
        }
        return this.caretPos;
    }

    public Integer getError() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getError();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return null;
    }

    public String getEditString() {
        if(this.ctrl != null){
            try {
                return new String(bbjInputE.getEditString(), StandardCharsets.UTF_8);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.editString;
    }

    public Boolean isHighlight() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getHighlight();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.highlight;
    }

    public Boolean isInsertMode() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getInsertMode();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.insert;
    }

    public Integer getLength() {
        if(this.ctrl != null){
            try {
                bbjInputE.getLength();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.length;
    }

    public Integer getMargin() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getMargin();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return -1;
    }

    public String getMask() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getMask();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.mask;
    }


    public String getPadCharacter() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPadCharacter();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.pad;
    }

    public Boolean isPassEnter() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPassEnter();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.passEnter;
    }

    public Boolean isPassTab() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getPassTab();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.passTab;
    }

    public String getRestore() {
        if(this.ctrl != null){
            try {
                return bbjInputE.getRestore();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this.restore;
    }



    public StringEditBox restore() {
        if(this.ctrl != null){
            try {
                bbjInputE.restore();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    public StringEditBox selectAll() {
        if(this.ctrl != null){
            try {
                bbjInputE.selectAll();
            } catch (BBjException e) {
                Environment.logError(e);
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
        return this.readOnly;
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



    @Override
    public StringEditBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public StringEditBox setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public StringEditBox setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public StringEditBox setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public StringEditBox setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public StringEditBox setId(String id){
        super.setId(id);
        return this;
    }

    @Override
    public StringEditBox setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public StringEditBox addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public StringEditBox removeClassName(String selector) {
        super.removeClassName(selector);
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
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        
        if(!this.callbacks.isEmpty()){
            this.editModifyEventSink = new StringEditBoxEditModifyEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.editModifyEventSink.addCallback(this.callbacks.remove(0));
            }
        }

        
        if(this.caretPos != 1){
            this.setCaretPos(this.caretPos);
        }

        if(!"".equals(this.editString)){
            this.setEditString(this.editString);
        }

        if(Boolean.TRUE.equals(this.highlight)){
            this.setHighlight(this.highlight);
        }

        if(Boolean.TRUE.equals(this.insert)){
            this.setInsertMode(this.insert);
        }

        if(this.length != null){
            this.setLength(this.length);
        }

        if(this.margin != 7){
            this.setMargin(this.margin);
        }

        if(!"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX".equals(this.mask)){
            this.setMask(this.mask);
        }

        if(!" ".equals(this.pad)){
            this.setPadCharacter(this.pad);
        }

        if(Boolean.TRUE.equals(this.passEnter)){
            this.setPassEnter(this.passEnter);
        }

        if(Boolean.TRUE.equals(this.passTab)){
            this.setPassTab(this.passTab);
        }

        if(!"".equals(this.restore)){
            this.setRestore(this.restore);
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
