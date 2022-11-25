package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.List;

public final class MultilineEdit extends AbstractDwcControl implements IReadOnly, ITextControl, IFocusable, IMouseWheelEnableable, IScrollable, ITabTraversable {

    private BBjCEdit bbjCEdit;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }
    
    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }


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




    public MultilineEdit(){
        this.readOnly = false;
        this.textHighlight = Highlight.HIGHLIGHT_NONE;
        this.horizontalScrollBarPosition = 0;
        this.verticalScrollBarPosition = 0;this.focusable = true;
        this.mouseWheelCondition = MouseWheelCondition.DEFAULT;
        this.tabTraversable = true;
        this.focusable = true;

    }


    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText());
            bbjCEdit = (BBjCEdit) ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Adds a paragraph to the MultilineEdit control
     * @param index - Specifies the paragraph number with 0 identifying the first paragraph. If index equals -1, paragraphs are added to the end.
     * @param paragraph - Specifies the paragraph to be added.
     * @return Returns this
     */
    public MultilineEdit addParagraph(Integer index, String paragraph) {
        if(this.ctrl != null){
            try {
                bbjCEdit.addParagraph(index, paragraph);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    // /**
    //  * Adds all paragraphs in the ArrayList MultilineEdit control
    //  * @param index - Specifies the 0-based index in which the paragraphs will be added.
    //  * @param paragraphs - Specifies an ArrayList that contains the paragraphs to be added
    //  * @return Returns this
    //  */
    // public void addParagraphs(Integer index, ArrayList<String> paragraphs) {
    //     try {
    //         bbjCEdit.addParagraphs(index, (BBjVector) paragraphs);
    //     } catch (BBjException e) {
    //         e.printStackTrace();
    //     }
    // }

    /**
     * Appends text to the end of a paragraph in the MultilineEdit control
     * @param index - Specifies the 0-based index of the paragraph to append the text to.
     * @param test - Specifies the text to be appended to the end of the paragraph.
     * @return Returns this
     */
    public MultilineEdit appendToParagraph(Integer parNum, String text) {
        if(this.ctrl != null){
            try {
                bbjCEdit.appendToParagraph(parNum, text);
            } catch (BBjException e) {
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
            }
        }
        return null;
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
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
                e.printStackTrace();
            }
        }
    }

    /**
     * Removes a paragraph in theMultilineEdit control
     * @param parIndex - Specifies the 0-based index of the paragraph to be removed.
     * @return Returns this
     */
    public MultilineEdit removeParagraph(Integer parIndex) {
        if(this.ctrl != null){
            try {
                bbjCEdit.removeParagraph(parIndex);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }



    /**
     * Sets whether a horizontal scroll bar will appear when the text is too large to fit within the MultilineEdit control
     * @param scroll - Specifies whether a horizontal scroll bar will appear when the text is too large to fit within control area (false = No Horizontal Scroll, true = Horizontal Scroll).
     * @return Returns this
     */
    public MultilineEdit setHorizontalScrollable(Boolean scroll) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setHorizontalScrollable(scroll);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setIgnoreEnters(Boolean ignore) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setIgnoreEnters(ignore);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setIgnoreTabs(Boolean ignore) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setIgnoreTabs(ignore);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setLimitToOneParagraph(Boolean limit) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLimitToOneParagraph(limit);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setLineCountLimit(Integer limit) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLineCountLimit(limit);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setLineWrap(Boolean wrap) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setLineWrap(wrap);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setMaxParagraphSize(Integer size) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setMaxParagraphSize(size);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setMaxLength(Integer length) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setMaxLength(length);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setOvertypeMode(Boolean overtype) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setOvertypeMode(overtype);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setTabSize(Integer size) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setTabSize(size);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setVerticalScrollable(Boolean scroll) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setVerticalScrollable(scroll);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setWrapStyleWord(Boolean word) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setWrapStyleWord(word);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.wrapWord = word;
        return this;
    }






    @Override
    public Boolean isReadOnly(){
        if(this.ctrl != null){
            try{
                return bbjCEdit.isEditable();
            } catch(BBjException e){
                e.printStackTrace();
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
    public MultilineEdit setReadOnly(Boolean editable) {
        if(this.ctrl != null){
            try {
                bbjCEdit.setEditable(editable);
            } catch (BBjException e) {
                e.printStackTrace();
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
    public MultilineEdit setHighlightOnFocus(Highlight highlight){
        if(this.ctrl != null){
            try{
                bbjCEdit.setHighlightOnFocus(highlight.highlight);
            } catch (BBjException e){
                e.printStackTrace();
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
                e.printStackTrace();
            }
        }
        return this.focusable;
    }

    @Override 
    public MultilineEdit setFocusable(Boolean focusable){
        if(this.ctrl != null){
            try{
                bbjCEdit.setFocusable(focusable);
            } catch(BBjException e){
                e.printStackTrace();
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
    public MultilineEdit setScrollWheelBehavior(MouseWheelCondition condition){
        if(this.ctrl != null){
            try{
                bbjCEdit.setScrollWheelBehavior(condition.mouseWheelEnabledCondition);
            } catch(BBjException e){
                e.printStackTrace();
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
    public MultilineEdit setHorizontalScrollBarPosition(Integer position){
        if(this.ctrl != null){
                bbjCEdit.setHorizontalScrollBarPosition(position);
        }
        this.horizontalScrollBarPosition = position;
        return this;
    }

    @Override
    public MultilineEdit setVerticalScrollBarPosition(Integer position){
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
                e.printStackTrace();
            }
        }
        return this.tabTraversable;
    }

    @Override
    public MultilineEdit setTabTraversable(Boolean traversable){
        if(this.ctrl != null){
            try{
                bbjCEdit.setTabTraversable(traversable);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traversable;
        return this;
    }



    public MultilineEdit setText(String text) {
        super.setControlText(text);
        return this;
    }

    public MultilineEdit setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public MultilineEdit setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public MultilineEdit setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public MultilineEdit setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public MultilineEdit setID(String id){
        super.setControlID(id);
        return this;
    }

    public MultilineEdit setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public MultilineEdit addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public MultilineEdit removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }








    public MultilineEdit setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public MultilineEdit setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }


    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();

        if(this.hScroll != false){
            this.setHorizontalScrollable(this.hScroll);
        }

        if(this.ignoreEnter != false){
            this.setIgnoreEnters(this.ignoreEnter);
        }

        if(this.ignoreTab != false){
            this.setIgnoreTabs(this.ignoreTab);
        }

        if(this.oneParagraph != false){
            this.setLimitToOneParagraph(this.oneParagraph);
        }

        if(this.lineLimit != null){
            this.setLineCountLimit(this.lineLimit);
        }

        if(this.lineWrap != false){
            this.setLineWrap(this.lineWrap);
        }

        if(this.maxParagraphSize != null){
            this.setMaxParagraphSize(this.maxParagraphSize);
        }

        if(this.maxLength != Integer.MAX_VALUE){
            this.setMaxLength(this.maxLength);
        }

        if(this.overtype != false){
            this.setOvertypeMode(this.overtype);
        }

        if(this.tabSize != 8){
            this.setTabSize(this.tabSize);
        }

        if(this.vScroll != false){
            this.setVerticalScrollable(this.vScroll);
        }

        if(this.wrapWord != true){
            this.setWrapStyleWord(this.wrapWord);
        }


        if(this.readOnly != false){
            this.setReadOnly(this.readOnly);
        }

        if(this.textHighlight != Highlight.HIGHLIGHT_NONE){
            this.setHighlightOnFocus(this.textHighlight);
        }

        if(this.focusable != true){
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

        if(this.tabTraversable != true){
            this.setTabTraversable(this.tabTraversable);

        }

    }


}
