package org.dwcj.component.textarea;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.Focusable;
import org.dwcj.component.HasMouseWheelCondition;
import org.dwcj.component.HasReadOnly;
import org.dwcj.component.Scrollable;
import org.dwcj.component.TabTraversable;
import org.dwcj.component.TextHighlightable;
import org.dwcj.component.panels.AbstractPanel;
import org.dwcj.component.textarea.event.TextAreaOnEditModifyEvent;
import org.dwcj.component.textarea.sinks.TextAreaOnEditModifyEventSink;
import org.dwcj.util.BBjFunctionalityHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

public final class TextArea extends AbstractDwcComponent implements HasReadOnly, TextHighlightable, Focusable, HasMouseWheelCondition, Scrollable, TabTraversable {

    private BBjCEdit bbjCEdit;

    public enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    private ArrayList<Consumer<TextAreaOnEditModifyEvent>> callbacks = new ArrayList<>();
    private TextAreaOnEditModifyEventSink editModifyEventSink;


    private Boolean hScroll = false;
    private Boolean ignoreEnter = false;
    private Boolean ignoreTab = false;
    private Boolean oneParagraph = false;
    private Integer lineLimit = null;
    private Boolean lineWrap = false;
    private Integer maxParagraphSize = null;
    /* Using Integer instead of long, as BBj setMaxLength() method doesn't support > 32 bit values  */
    private Integer maxLength = Integer.MAX_VALUE;
    private Boolean overtype = false;
    private Integer tabSize = 8;
    private Boolean vScroll = false;
    private Boolean wrapWord = true;




    public TextArea(){
        this.readOnly = false;
        this.textHighlight = Highlight.HIGHLIGHT_NONE;
        this.horizontalScrollBarPosition = 0;
        this.verticalScrollBarPosition = 0;
        this.focusable = true;
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
        this.tabTraversable = true;

    }


    @Override
    protected void create(AbstractPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            byte [] flags = BBjFunctionalityHelper.buildStandardCreationFlags(this.isVisible(), this.isEnabled());
            ctrl = w.addCEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText(), flags);
            bbjCEdit = (BBjCEdit) ctrl;
            catchUp();
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public TextArea onEditModify(Consumer<TextAreaOnEditModifyEvent> callback){
        if(this.ctrl != null){
            if(this.editModifyEventSink == null){
                this.editModifyEventSink = new TextAreaOnEditModifyEventSink(this);
            }
            this.editModifyEventSink.addCallback(callback);
        }
        else{
            this.callbacks.add(callback);
        }
        return this;
    }

    /**
     * Adds a paragraph to the MultilineEdit control
     * @param index - Specifies the paragraph number with 0 identifying the first paragraph. If index equals -1, paragraphs are added to the end.
     * @param paragraph - Specifies the paragraph to be added.
     * @return Returns this
     */
    public TextArea addParagraph(Integer index, String paragraph) {
        if(this.ctrl != null){
            try {
                bbjCEdit.addParagraph(index, paragraph);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

    
    /**
     * Appends text to the end of a paragraph in the MultilineEdit control
     * @param index - Specifies the 0-based index of the paragraph to append the text to.
     * @param test - Specifies the text to be appended to the end of the paragraph.
     * @return Returns this
     */
    public TextArea appendToParagraph(Integer parNum, String text) {
        if(this.ctrl != null){
            try {
                bbjCEdit.appendToParagraph(parNum, text);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }

     /**
     *  This method returns the text of all paragraphs from the MultilineEdit in a List
     * @return Paragraph text from the MultilineEdit control in a List of strings.
     */   
    public List<String> getAllParagraphs() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getAllParagraphs();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return new ArrayList<>();
    }

    //Will likely eventually want to add an internal counter for tracking para num.

    /**
     * This method returns the current zero-based paragraph index in the MultilineEdit control
     * @return Returns the zero-based paragraph index in the MultilineEdit control
     */
    public Integer getCurrentParagraphIndex() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getCurrentParagraphIndex();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return -1;
    }

    /**
     * Returns whether a horizontal scroll bar will appear when the text is too large to fit within the  MultilineEdit control area
     * @return Returns whether a horizontal scroll bar will appear when the text is too large to fit within the control area (false = No Horizontal Scroll Bar, true = Horizontal Scroll Bar).
     */
    public Boolean isHorizontalScrollable() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getHorizontalScrollable();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * This method returns whether the ENTER key is ignored in the MultilineEdit control
     * @return Returns whether the ENTER key is ignored in the MultilineEdit control (false = ENTER key not ignored, true = ENTER key ignored).
     */
    public Boolean isIgnoreEnters() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getIgnoreEnters();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * Returns whether TAB key is ignored in the MultilineEdit control
     * @return Returns whether the TAB key is ignored in the control (false = TAB key not ignored, true = TAB key ignored).
     */
    public Boolean isIgnoreTabs() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getIgnoreTabs();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * Returns whether the MultilineEdit control is limited to one paragraph.
     * @return Returns whether the control is limited to one paragraph (false = Not limited, true = Limited).
     */
    public Boolean isLimitToOneParagraph() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getLimitToOneParagraph();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * This method returns the maximum number of lines (paragraphs) that can be entered into the MultilineEdit control
     * @return Returns the maximum number of lines that can be entered into the control (0 = no limit)
     */
    public Integer getLineCountLimit() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getLineCountLimit();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return -1;
    }

    /**
     * Returns whether lines are wrapped in the MultilineEdit control
     * @return Returns whether the lines are wrapped in the control (false = Not Wrapped, true = Wrapped).
     */
    public Boolean isLineWrap() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getLineWrap();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * Returns the maximum number of characters allowed per paragraph in the MultilineEdit control
     * @return Returns the maximum number of characters allowed per paragraph in the control
     */
    public Integer getMaxParagraphSize() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getMaxParagraphSize();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return -1;
    }

    /**
     * This method returns the maximum length of the text in a MultilineEdit control
     * @return Returns the maximum length of the text in the control.
     */
    public Integer getMaxLength() {
        if(this.ctrl != null){
            return bbjCEdit.getMaxLength();
        }
        return 2147483647;
    }

    /**
     * Returns the number of paragraphs in the MultilineEdit control
     * @return Returns the number of paragraphs in the control.
     */
    public Integer getNumberOfParagraphs() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getNumberOfParagraphs();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return 0;
    }

    /**
     * Returns whether overtype mode is used in the MultilineEdit control
     * @return Returns whether overtype mode is being used in the control (false = Not in Overtype Mode, true = In Overtype Mode).
     */
    public Boolean isOvertypeMode() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getOvertypeMode();
            } catch (BBjException e) {
                Environment.logError(e);
            }

        }
        return false;
    }

    /**
     * Returns the paragraph text in the MultilineEdit control
     * @param parNum - Specifies the 0-based index of the paragraph, for text to be returned.
     * @return Returns the paragraph text in the control.
     */
    public String getParagraph(Integer parNum) {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getParagraph(parNum);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return "";
    }

    /**
     * Returns the selected text in the MultilineEdit control
     * @return Returns a four-element List of strings that identifies 
     * the currently selected text in the MultilineEdit control. 
     * If the List is selectionList, then selectionList.getItem(0) 
     * and selectionList.getItem(1) identify the paragraph and offset 
     * of the beginning of the selected text while selectionList.getItem(2) 
     * and selectionList.getItem(3) identify the paragraph and offset of 
     * the end of the selected text. All values are zero-based. If the 
     * start and end points are the same, there is no selection and the 
     * location of the caret (insertion point) is reported.
     */
    public List<String> getSelection() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getSelection();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return Collections.emptyList();
    }

    /**
     * Returns the tab size in the MultilineEdit control
     * @return Returns the tab size of the control.
     */
    public Integer getTabSize() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getTabSize();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return 8;
    }

    /**
     * Returns whether a vertical scroll bar will appear when the text is too large to fit within the MultilineEdit control area.
     * @return Returns whether a vertical scroll bar will appear when the text is too large to fit within control area (false = No Vertical Scroll Bar, true = Vertical Scroll Bar).
     */
    public Boolean isVerticalScrollable() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getVerticalScrollable();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * This method gets the style of wrapping used if the MultilineEdit control is wrapping lines
     * @return Returns the style of wrapping used if the text area is wrapping lines (true = wrap at word boundaries; false = wrap at character boundaries).
     */
    public Boolean isWrapStyleWord() {
        if(this.ctrl != null){
            try {
                return bbjCEdit.getWrapStyleWord();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return false;
    }

    /**
     * Highlights characters in the MultilineEdit control
     * @param parIndex1 - Specifies the 0-based paragraph index where the highlight is to begin.
     * @param off1 - Specifies the offset of characters where the highlight is to begin.
     * @param parIndex2 - Specifies the 0-based paragraph index where the highlight is to end.
     * @param off2 - Specifies the offset of characters where the highlight is to end.
     */
    public void highlight(Integer parIndex1, Integer off1, Integer parIndex2, Integer off2) {
        if(this.ctrl != null){
            try {
                bbjCEdit.highlight(parIndex1, off1, parIndex2, off2);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
    }

    /**
     * Returns whether the text is editable in the MultilineEdit control
     * @return Returns whether the text is editable in the control (false = Not Editable, true = Editable).
     */

    /**
     * This method removes all paragraphs from the MultilineEdit control
     */
    public void removeAll() {
        if(this.ctrl != null){
            try {
                bbjCEdit.removeAll();
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
    }

    /**
     * Removes a paragraph in theMultilineEdit control
     * @param parIndex - Specifies the 0-based index of the paragraph to be removed.
     * @return Returns this
     */
    public TextArea removeParagraph(Integer parIndex) {
        if(this.ctrl != null){
            try {
                bbjCEdit.removeParagraph(parIndex);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        return this;
    }



    /**
     * Sets whether a horizontal scroll bar will appear when the text is too large to fit within the MultilineEdit control
     * @param scroll - Specifies whether a horizontal scroll bar will appear when the text is too large to fit within control area (false = No Horizontal Scroll, true = Horizontal Scroll).
     * @return Returns this
     */
    public TextArea setHorizontalScrollable(Boolean scroll) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setHorizontalScrollable(scroll);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.hScroll = scroll;
        return this;
    }

    /**
     * This method sets whether to ignore the ENTER key in the MultilineEdit control
     * @param ignore - Specifies whether the ENTER key is ignored (false = ENTER key not ignored, true = ENTER key ignored).
     * @return Returns this
     */
    public TextArea setIgnoreEnters(Boolean ignore) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setIgnoreEnters(ignore);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.ignoreEnter = ignore;
        return this;
    }

    /**
     * Sets whether to ignore the Tab key in the MultilineEdit control
     * @param ignore - Specifies whether the TAB key is ignored (false = TAB key not ignored, true = TAB key ignored)
     * @return Returns this
     */
    public TextArea setIgnoreTabs(Boolean ignore) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setIgnoreTabs(ignore);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.ignoreTab = ignore;
        return this;
    }

    /**
     * Sets whether the MultilineEdit control is limited to one paragraph
     * @param limit - Specifies whether control is limited to one paragraph (false = Not limited, true = Limited)
     * @return Returns this
     */
    public TextArea setLimitToOneParagraph(Boolean limit) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLimitToOneParagraph(limit);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.oneParagraph = limit;
        return this;
    }

    /**
     * This method sets the maximum number of paragraphs (lines) that can be entered into the MultilineEdit control
     * @param limit - Maximum number of lines (paragraphs) that can be entered into the control.
     * @return Returns this
     */
    public TextArea setLineCountLimit(Integer limit) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLineCountLimit(limit);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.lineLimit = limit;
        return this;
    }

    /**
     * Sets whether the lines will be wrapped in the MultilineEdit control
     * @param wrap - Specifies whether the lines will be wrapped (false = Not Wrapped, true = Wrapped)
     * @return Returns this
     */
    public TextArea setLineWrap(Boolean wrap) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLineWrap(wrap);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.lineWrap = wrap;
        return this;
    }

    /**
     * Sets the maximum number of characters allowed in a MultilineEdit control paragraph.
     * @param limit - Specifies the number of characters to be allowed in a paragraph.
     * @return Returns this
     */
    public TextArea setMaxParagraphSize(Integer size) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setMaxParagraphSize(size);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.maxParagraphSize = size;
        return this;
    }

    /**
     * This method sets the maximum length of the text in a MultilineEdit control
     * @param limit - Specifies the maximum length of the text in the control.
     * @return Returns this
     */
    public TextArea setMaxLength(Integer length) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setMaxLength(length);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.maxLength = length;
        return this;
    }

    /**
     * Sets whether the overtype mode is used in the MultilineEdit control
     * @param overtype - Specifies whether to use overtype mode (false = Not in Overtype Mode, true = In Overtype Mode)
     * @return Returns this
     */
    public TextArea setOvertypeMode(Boolean overtype) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setOvertypeMode(overtype);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.overtype = overtype;
        return this;
    }

    /**
     * Sets the tab size of the MultilineEdit control
     * @param size - Specifies the tab size.
     * @return Returns this
     */
    public TextArea setTabSize(Integer size) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setTabSize(size);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.tabSize = size;
        return this;
    }

    /**
     * Sets whether a vertical scroll bar will appear when the text is too large to fit within the MultilineEdit control
     * @param scroll - Specifies whether a vertical scroll bar will appear when the text is too large to fit within control area (false = No Vertical Scroll Bar, true = Vertical Scroll Bar)
     * @return Returns this
     */
    public TextArea setVerticalScrollable(Boolean scroll) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setVerticalScrollable(scroll);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.vScroll = scroll;
        return this;
    }

    /**
     * this method sets the style of wrapping used if the MultilineEdit control is wrapping lines.
     * @param wrap - Sets the style of wrapping used if the BBjCEdit is wrapping lines (false = Wrap at character boundaries, true = Wrap at word boundaries.)
     * @return Returns this
     */
    public TextArea setWrapStyleWord(Boolean word) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setWrapStyleWord(word);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.wrapWord = word;
        return this;
    }






    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return !bbjCEdit.isEditable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.readOnly;
    }

    /**
     * Sets whether the text is editable in the MultilineEdit control
     * @param editable - Specifies whether the text can be edited (false = Not Editable, true = Editable).
     * @return Returns this
     */
    @Override
    public TextArea setReadOnly(Boolean editable) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setEditable(!editable);
            } catch (BBjException e) {
                Environment.logError(e);
            }
        }
        this.readOnly = editable;
        return this;
    }


    @Override
    public Highlight getHighlightOnFocus(){
        return this.textHighlight;
    } 

    @Override
    public TextArea setHighlightOnFocus(Highlight highlight){
        if(this.ctrl != null){
            try{
                bbjCEdit.setHighlightOnFocus(highlight.highlightType);
            } catch (BBjException e){
                Environment.logError(e);
            }
        }
        this.textHighlight = highlight;
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                bbjCEdit.isFocusable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.focusable;
    }

    @Override 
    public TextArea setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjCEdit.setFocusable(focusable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.focusable = focusable;
        return this;
    }

    @Override 
    public MouseWheelCondition getScrollWheelBehavior(){
        return this.mouseWheelCondition;
    }

    @Override
    public TextArea setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                bbjCEdit.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.mouseWheelCondition = condition;
        return this;
    }

    @Override
    public Integer getHorizontalScrollBarHeight(){
        if(this.ctrl != null){
                bbjCEdit.getHorizontalScrollBarHeight();
        }
        return 0;

    }

    @Override
    public Integer getHorizontalScrollBarPosition(){
        if(this.ctrl != null){
                bbjCEdit.getHorizontalScrollBarPosition();
        }
        return this.verticalScrollBarPosition;

    }

    @Override
    public Integer getHorizontalScrollBarWidth(){
        if(this.ctrl != null){
                bbjCEdit.getHorizontalScrollBarWidth();
        }
        return 0;

    }

    @Override
    public Integer getVerticalScrollBarHeight(){
        if(this.ctrl != null){
                bbjCEdit.getVerticalScrollBarHeight();
        }
        return 0;

    }

    @Override
    public Integer getVerticalScrollBarPosition(){
        if(this.ctrl != null){
                bbjCEdit.getVerticalScrollBarPosition();
        }
        return this.horizontalScrollBarPosition;

    }

    @Override
    public Integer getVerticalScrollBarWidth(){
        if(this.ctrl != null){
                bbjCEdit.getVerticalScrollBarWidth();
        }
        return 0;

    }

    @Override
    public Boolean isHorizontalScrollBarVisible(){
        if(this.ctrl != null){
                bbjCEdit.isHorizontalScrollBarVisible();
        }
        return false;

    }

    @Override
    public Boolean isVerticalScrollBarVisible(){
        if(this.ctrl != null){
                bbjCEdit.isVerticalScrollBarVisible();
        }
        return false;

    }

    @Override
    public TextArea setHorizontalScrollBarPosition(Integer position){
        if(this.ctrl != null){
                bbjCEdit.setHorizontalScrollBarPosition(position);
        }
        this.horizontalScrollBarPosition = position;
        return this;
    }

    @Override
    public TextArea setVerticalScrollBarPosition(Integer position){
        if(this.ctrl != null){
                bbjCEdit.setVerticalScrollBarPosition(position);
        }
        this.verticalScrollBarPosition = position;
        return this;
    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                bbjCEdit.isTabTraversable();
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        return this.tabTraversable;
    }

    @Override
    public TextArea setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjCEdit.setTabTraversable(traversable);
            } catch(BBjException e){
                Environment.logError(e);
            }
        }
        this.tabTraversable = traversable;
        return this;
    }



    @Override
    public TextArea setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public TextArea setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public TextArea setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public TextArea setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public TextArea setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public TextArea setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public TextArea setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public TextArea addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public TextArea removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }








    public TextArea setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public TextArea setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }


    @Override
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();

        if(!this.callbacks.isEmpty()){
            this.editModifyEventSink = new TextAreaOnEditModifyEventSink(this);
            while(!this.callbacks.isEmpty()){
                this.editModifyEventSink.addCallback(this.callbacks.remove(0));
            }
        }

        if(Boolean.TRUE.equals(this.hScroll)){
            this.setHorizontalScrollable(this.hScroll);
        }

        if(Boolean.TRUE.equals(this.ignoreEnter)){
            this.setIgnoreEnters(this.ignoreEnter);
        }

        if(Boolean.TRUE.equals(this.ignoreTab)){
            this.setIgnoreTabs(this.ignoreTab);
        }

        if(Boolean.TRUE.equals(this.oneParagraph)){
            this.setLimitToOneParagraph(this.oneParagraph);
        }

        if(this.lineLimit != null){
            this.setLineCountLimit(this.lineLimit);
        }

        if(Boolean.TRUE.equals(this.lineWrap)){
            this.setLineWrap(this.lineWrap);
        }

        if(this.maxParagraphSize != null){
            this.setMaxParagraphSize(this.maxParagraphSize);
        }

        if(this.maxLength != Integer.MAX_VALUE){
            this.setMaxLength(this.maxLength);
        }

        if(Boolean.TRUE.equals(this.overtype)){
            this.setOvertypeMode(this.overtype);
        }

        if(this.tabSize != 8){
            this.setTabSize(this.tabSize);
        }

        if(Boolean.TRUE.equals(this.vScroll)){
            this.setVerticalScrollable(this.vScroll);
        }

        if(Boolean.FALSE.equals(this.wrapWord)){
            this.setWrapStyleWord(this.wrapWord);
        }


        if(Boolean.TRUE.equals(this.readOnly)){
            this.setReadOnly(this.readOnly);
        }

        if(this.textHighlight != Highlight.HIGHLIGHT_NONE){
            this.setHighlightOnFocus(this.textHighlight);
        }

        if(Boolean.FALSE.equals(this.focusable)){
            this.setFocusable(this.focusable);
        }

        if(this.horizontalScrollBarPosition != 0){
            this.setHorizontalScrollBarPosition(this.horizontalScrollBarPosition);
        }

        if(this.verticalScrollBarPosition != 0){
            this.setVerticalScrollBarPosition(this.horizontalScrollBarPosition);
        }

        if(this.mouseWheelCondition != MouseWheelCondition.DEFAULT){
            this.setScrollWheelBehavior(this.mouseWheelCondition);
        }

        if(Boolean.FALSE.equals(this.tabTraversable)){
            this.setTabTraversable(this.tabTraversable);

        }

    }


}
