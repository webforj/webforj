package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.ArrayList;
import java.util.List;

public final class MultilineEdit extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private BBjCEdit bbjCEdit;


    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCEdit(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText());
            catchUp();
            bbjCEdit = (BBjCEdit) ctrl;
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
    public MultilineEdit addParagraph(int index, String paragraph) {
        try {
            bbjCEdit.addParagraph(index, paragraph);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Adds all paragraphs in the ArrayList MultilineEdit control
     * @param index - Specifies the 0-based index in which the paragraphs will be added.
     * @param paragraphs - Specifies an ArrayList that contains the paragraphs to be added
     * @return Returns this
     */
    public void addParagraphs(int index, ArrayList<String> paragraphs) {
        try {
            bbjCEdit.addParagraphs(index, (BBjVector) paragraphs);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * Appends text to the end of a paragraph in the MultilineEdit control
     * @param index - Specifies the 0-based index of the paragraph to append the text to.
     * @param test - Specifies the text to be appended to the end of the paragraph.
     * @return Returns this
     */
    public MultilineEdit appendToParagraph(int parNum, String text) {
        try {
            bbjCEdit.appendToParagraph(parNum, text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

     /**
     *  This method returns the text of all paragraphs from the MultilineEdit in a List
     * @return Paragraph text from the MultilineEdit control in a List of strings.
     */   
    public List<String> getAllParagraphs() {
        try {
            return bbjCEdit.getAllParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    /**
     * This method returns the current zero-based paragraph index in the MultilineEdit control
     * @return Returns the zero-based paragraph index in the MultilineEdit control
     */
    public int getCurrentParagraphIndex() {
        try {
            return bbjCEdit.getCurrentParagraphIndex();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether a horizontal scroll bar will appear when the text is too large to fit within the  MultilineEdit control area
     * @return Returns whether a horizontal scroll bar will appear when the text is too large to fit within the control area (false = No Horizontal Scroll Bar, true = Horizontal Scroll Bar).
     */
    public boolean getHorizontalScrollable() {
        try {
            return bbjCEdit.getHorizontalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method returns whether the ENTER key is ignored in the MultilineEdit control
     * @return Returns whether the ENTER key is ignored in the MultilineEdit control (false = ENTER key not ignored, true = ENTER key ignored).
     */
    public boolean getIgnoreEnters() {
        try {
            return bbjCEdit.getIgnoreEnters();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether TAB key is ignored in the MultilineEdit control
     * @return Returns whether the TAB key is ignored in the control (false = TAB key not ignored, true = TAB key ignored).
     */
    public boolean getIgnoreTabs() {
        try {
            return bbjCEdit.getIgnoreTabs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether the MultilineEdit control is limited to one paragraph.
     * @return Returns whether the control is limited to one paragraph (false = Not limited, true = Limited).
     */
    public boolean getLimitToOneParagraph() {
        try {
            return bbjCEdit.getLimitToOneParagraph();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method returns the maximum number of lines (paragraphs) that can be entered into the MultilineEdit control
     * @return Returns the maximum number of lines that can be entered into the control (0 = no limit)
     */
    public int getLineCountLimit() {
        try {
            return bbjCEdit.getLineCountLimit();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether lines are wrapped in the MultilineEdit control
     * @return Returns whether the lines are wrapped in the control (false = Not Wrapped, true = Wrapped).
     */
    public boolean getLineWrap() {
        try {
            return bbjCEdit.getLineWrap();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns the maximum number of characters allowed per paragraph in the MultilineEdit control
     * @return Returns the maximum number of characters allowed per paragraph in the control
     */
    public int getMaxParagraphSize() {
        try {
            return bbjCEdit.getMaxParagraphSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the maximum length of the text in a MultilineEdit control
     * @return Returns the maximum length of the text in the control.
     */
    public int getMaxLength() {
        return bbjCEdit.getMaxLength();
    }

    /**
     * Returns the number of paragraphs in the MultilineEdit control
     * @return Returns the number of paragraphs in the control.
     */
    public int getNumberOfParagraphs() {
        try {
            return bbjCEdit.getNumberOfParagraphs();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether overtype mode is used in the MultilineEdit control
     * @return Returns whether overtype mode is being used in the control (false = Not in Overtype Mode, true = In Overtype Mode).
     */
    public boolean getOvertypeMode() {
        try {
            return bbjCEdit.getOvertypeMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns the paragraph text in the MultilineEdit control
     * @param parNum - Specifies the 0-based index of the paragraph, for text to be returned.
     * @return Returns the paragraph text in the control.
     */
    public String getParagraph(int parNum) {
        try {
            return bbjCEdit.getParagraph(parNum);
        } catch (BBjException e) {
            e.printStackTrace();
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
        try {
            return bbjCEdit.getSelection();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    /**
     * Returns the tab size in the MultilineEdit control
     * @return Returns the tab size of the control.
     */
    public int getTabSize() {
        try {
            return bbjCEdit.getTabSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether a vertical scroll bar will appear when the text is too large to fit within the MultilineEdit control area.
     * @return Returns whether a vertical scroll bar will appear when the text is too large to fit within control area (false = No Vertical Scroll Bar, true = Vertical Scroll Bar).
     */
    public boolean getVerticalScrollable() {
        try {
            return bbjCEdit.getVerticalScrollable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method gets the style of wrapping used if the MultilineEdit control is wrapping lines
     * @return Returns the style of wrapping used if the text area is wrapping lines (true = wrap at word boundaries; false = wrap at character boundaries).
     */
    public boolean getWrapStyleWord() {
        try {
            return bbjCEdit.getWrapStyleWord();
        } catch (BBjException e) {
            e.printStackTrace();
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
    public void highlight(int parIndex1, int off1, int parIndex2, int off2) {
        try {
            bbjCEdit.highlight(parIndex1, off1, parIndex2, off2);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns whether the text is editable in the MultilineEdit control
     * @return Returns whether the text is editable in the control (false = Not Editable, true = Editable).
     */
    public boolean isEditable() {
        try {
            return bbjCEdit.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method removes all paragraphs from the MultilineEdit control
     */
    public void removeAll() {
        try {
            bbjCEdit.removeAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * Removes a paragraph in theMultilineEdit control
     * @param parIndex - Specifies the 0-based index of the paragraph to be removed.
     * @return Returns this
     */
    public MultilineEdit removeParagraph(int parIndex) {
        try {
            bbjCEdit.removeParagraph(parIndex);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the text is editable in the MultilineEdit control
     * @param editable - Specifies whether the text can be edited (false = Not Editable, true = Editable).
     * @return Returns this
     */
    public MultilineEdit setEditable(boolean editable) {
        try {
            bbjCEdit.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether a horizontal scroll bar will appear when the text is too large to fit within the MultilineEdit control
     * @param scroll - Specifies whether a horizontal scroll bar will appear when the text is too large to fit within control area (false = No Horizontal Scroll, true = Horizontal Scroll).
     * @return Returns this
     */
    public MultilineEdit setHorizontalScrollable(boolean scroll) {
        try {
            bbjCEdit.setHorizontalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets whether to ignore the ENTER key in the MultilineEdit control
     * @param ignore - Specifies whether the ENTER key is ignored (false = ENTER key not ignored, true = ENTER key ignored).
     * @return Returns this
     */
    public MultilineEdit setIgnoreEnters(boolean ignore) {
        try {
            bbjCEdit.setIgnoreEnters(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether to ignore the Tab key in the MultilineEdit control
     * @param ignore - Specifies whether the TAB key is ignored (false = TAB key not ignored, true = TAB key ignored)
     * @return Returns this
     */
    public MultilineEdit setIgnoreTabs(boolean ignore) {
        try {
            bbjCEdit.setIgnoreTabs(ignore);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the MultilineEdit control is limited to one paragraph
     * @param limit - Specifies whether control is limited to one paragraph (false = Not limited, true = Limited)
     * @return Returns this
     */
    public MultilineEdit setLimitToOneParagraph(boolean limit) {
        try {
            bbjCEdit.setLimitToOneParagraph(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the maximum number of paragraphs (lines) that can be entered into the MultilineEdit control
     * @param limit - Maximum number of lines (paragraphs) that can be entered into the control.
     * @return Returns this
     */
    public MultilineEdit setLineCountLimit(int limit) {
        try {
            bbjCEdit.setLineCountLimit(limit);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the lines will be wrapped in the MultilineEdit control
     * @param wrap - Specifies whether the lines will be wrapped (false = Not Wrapped, true = Wrapped)
     * @return Returns this
     */
    public MultilineEdit setLineWrap(boolean wrap) {
        try {
            bbjCEdit.setLineWrap(wrap);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the maximum number of characters allowed in a MultilineEdit control paragraph.
     * @param limit - Specifies the number of characters to be allowed in a paragraph.
     * @return Returns this
     */
    public MultilineEdit setMaxParagraphSize(int size) {
        try {
            bbjCEdit.setMaxParagraphSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the maximum length of the text in a MultilineEdit control
     * @param limit - Specifies the maximum length of the text in the control.
     * @return Returns this
     */
    public MultilineEdit setMaxLength(int length) {
        try {
            bbjCEdit.setMaxLength(length);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the overtype mode is used in the MultilineEdit control
     * @param overtype - Specifies whether to use overtype mode (false = Not in Overtype Mode, true = In Overtype Mode)
     * @return Returns this
     */
    public MultilineEdit setOvertypeMode(boolean overtype) {
        try {
            bbjCEdit.setOvertypeMode(overtype);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the tab size of the MultilineEdit control
     * @param size - Specifies the tab size.
     * @return Returns this
     */
    public MultilineEdit setTabSize(int size) {
        try {
            bbjCEdit.setTabSize(size);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether a vertical scroll bar will appear when the text is too large to fit within the MultilineEdit control
     * @param scroll - Specifies whether a vertical scroll bar will appear when the text is too large to fit within control area (false = No Vertical Scroll Bar, true = Vertical Scroll Bar)
     * @return Returns this
     */
    public MultilineEdit setVerticalScrollable(boolean scroll) {
        try {
            bbjCEdit.setVerticalScrollable(scroll);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * this method sets the style of wrapping used if the MultilineEdit control is wrapping lines.
     * @param wrap - Sets the style of wrapping used if the BBjCEdit is wrapping lines (false = Wrap at character boundaries, true = Wrap at word boundaries.)
     * @return Returns this
     */
    public MultilineEdit setWrapStyleWord(boolean word) {
        try {
            bbjCEdit.setWrapStyleWord(word);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public IExpansible setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
