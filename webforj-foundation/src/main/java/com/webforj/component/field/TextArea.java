package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjCEdit;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.bridge.WindowAccessor;
import com.webforj.component.window.Window;
import com.webforj.concern.HasMaxLength;
import com.webforj.concern.HasMinLength;
import com.webforj.concern.HasTypingMode;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.selection.SelectionRange;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.BBjFunctionalityHelper;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * A text area component that allows the user to enter multiple lines of text.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
public final class TextArea extends DwcField<TextArea, String> implements HasTypingMode<TextArea>,
    HasMinLength<TextArea>, HasMaxLength<TextArea>{
  private List<String> paragraphs = new ArrayList<>();
  private int rows = 2;
  private int columns = 20;
  private int lineCountLimit = Integer.MAX_VALUE;
  private int paragraphLengthLimit = Integer.MAX_VALUE;
  private int maxLength = Integer.MAX_VALUE;
  private int minLength = 0;
  private boolean lineWrap = false;
  private boolean horizontalScroll = false;
  private boolean verticalScroll = false;
  private WrapStyle wrapStyle = WrapStyle.WORD_BOUNDARIES;
  private TypingMode typingMode = TypingMode.INSERT;
  private SelectionRange range = null;

  /**
   * Describes the style of wrapping used if the TextArea is wrapping lines.
   */
  public enum WrapStyle {
    /**
     * Wrap at character boundaries.
     */
    CHARACTER_BOUNDARIES,
    /**
     * Wrap at word boundaries.
     */
    WORD_BOUNDARIES
  }

  /**
   * Creates a new textarea component instance.
   *
   * @param label Specifies the label of the component.
   * @param value Specifies the initial value of the component.
   * @param rows Specifies the number of visible text lines for the component.
   * @param columns Specifies the visible width of the component, in average character widths.
   * @param valueChangeListener a listener to be notified when the component's value is changed.
   */
  public TextArea(String label, String value, int rows, int columns,
      EventListener<ValueChangeEvent<String>> valueChangeListener) {
    super();
    setLabel(label);
    setValue(value);
    setRows(rows);
    setColumns(columns);

    if (valueChangeListener != null) {
      addValueChangeListener(valueChangeListener);
    }

    setVerticalScroll(true);
    setLineWrap(true);
  }

  /**
   * Creates a new textarea component instance.
   *
   * @param label - Specifies the label of the component.
   * @param value - Specifies the initial value of the component.
   * @param valueChangeListener a listener to be notified when the component's value is changed.
   */
  public TextArea(String label, String value,
      EventListener<ValueChangeEvent<String>> valueChangeListener) {
    this(label, value, 2, 20, valueChangeListener);
  }

  /**
   * Creates a new textarea component instance.
   *
   * @param label - Specifies the label of the component.
   * @param value - Specifies the initial value of the component.
   */
  public TextArea(String label, String value) {
    this(label, value, null);
  }

  /**
   * Creates a new textarea component instance.
   *
   * @param label - Specifies the label of the component.
   */
  public TextArea(String label) {
    this(label, "");
  }

  /**
   * Creates a new instance of the text area component.
   */
  public TextArea() {
    this("");
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TextArea setValue(String value) {
    setText(value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return getText();
  }

  /**
   * Adds a text paragraph at the specified index.
   *
   * @param index - Specifies the paragraph number with 0 identifying the first paragraph. If index
   *        less than 0, the paragraph is added at the end of the text.
   * @param paragraph - Specifies the paragraph to be added.
   * @return the component itself.
   */
  public TextArea addParagraph(int index, String paragraph) {
    BBjCEdit field = inferField();
    int indexToUse = index < 0 ? -1 : index;

    if (field != null) {
      try {
        field.addParagraph(index, paragraph);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      if (indexToUse < 0) {
        paragraphs.add(paragraph);
      } else {
        if (indexToUse > paragraphs.size()) {
          indexToUse = paragraphs.size();
        }

        paragraphs.add(indexToUse, paragraph);
      }
    }

    return this;
  }

  /**
   * Adds a text paragraph at the end of the component's text.
   *
   * @param paragraph - Specifies the paragraph to be added.
   * @return the component itself.
   */
  public TextArea addParagraph(String paragraph) {
    return addParagraph(-1, paragraph);
  }

  /**
   * Adds a list of paragraphs at the specified index.
   *
   * @param index - Specifies the paragraph number with 0 identifying the first paragraph. If index
   *        less than 0, the paragraph is added at the end of the text.
   * @param paragraphs - Specifies the paragraphs to be added.
   * @return the component itself.
   */
  public TextArea addParagraphs(int index, List<String> paragraphs) {
    List<String> reversed = new ArrayList<>(paragraphs);
    Collections.reverse(reversed);

    for (String paragraph : reversed) {
      addParagraph(index, paragraph);
    }

    return this;
  }

  /**
   * Adds a list of paragraphs at the end of the component's text.
   *
   * @param paragraphs - Specifies the paragraphs to be added.
   * @return the component itself.
   */
  public TextArea addParagraphs(List<String> paragraphs) {
    return addParagraphs(-1, paragraphs);
  }

  /**
   * Appends text to the specified paragraph.
   *
   * @param index - Specifies the paragraph number with 0 identifying the first paragraph.
   * @param text - Specifies the text to be appended.
   * @return the component itself.
   */
  public TextArea appendToParagraph(int index, String text) {
    if (index < 0) {
      throw new IllegalArgumentException("Index must be greater than or equal to 0");
    }

    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.appendToParagraph(index, text);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      if (index < paragraphs.size()) {
        String paragraph = paragraphs.get(index);
        paragraphs.set(index, paragraph + text);
      }
    }

    return this;
  }


  /**
   * Removes the paragraph at the specified index.
   *
   * @param index - Specifies the paragraph number with 0 identifying the first paragraph.
   * @return the component itself.
   */
  public TextArea removeParagraph(int index) {
    if (index < 0) {
      throw new IllegalArgumentException("Index must be greater than or equal to 0");
    }

    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.removeParagraph(index);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    } else {
      if (index < paragraphs.size()) {
        paragraphs.remove(index);
      }
    }

    return this;
  }

  /**
   * Gets the list of paragraphs.
   *
   * @return the list of paragraphs.
   */
  @SuppressWarnings("unchecked")
  public List<String> getParagraphs() {
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        List<String> ret = new ArrayList<>();
        field.getAllParagraphs().stream().forEach(p -> ret.add(String.valueOf(p)));
        return ret;
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return paragraphs;
  }

  /**
   * Sets the maximum number of lines that the text area can contain.
   *
   * @param lineCountLimit - Specifies the maximum number of lines.
   * @return the component itself.
   */
  public TextArea setLineCountLimit(int lineCountLimit) {
    if (lineCountLimit < 0) {
      throw new IllegalArgumentException("Line count limit must be greater than or equal to 0");
    }

    this.lineCountLimit = lineCountLimit;
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setLineCountLimit(lineCountLimit);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the maximum number of lines that the text area can contain.
   *
   * @return the maximum number of lines.
   */
  public int getLineCountLimit() {
    return lineCountLimit;
  }

  /**
   * Sets the maximum length of a paragraph.
   *
   * @param paragraphLengthLimit - Specifies the maximum length of a paragraph.
   * @return the component itself.
   */
  public TextArea setParagraphLengthLimit(int paragraphLengthLimit) {
    if (paragraphLengthLimit < 0) {
      throw new IllegalArgumentException("Max paragraph length must be greater than or equal to 0");
    }

    this.paragraphLengthLimit = paragraphLengthLimit;
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setMaxParagraphSize(paragraphLengthLimit);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the maximum length of a paragraph.
   *
   * @return the maximum length of a paragraph.
   */
  public int getParagraphLengthLimit() {
    return paragraphLengthLimit;
  }

  /**
   * Sets the maximum length of the text.
   *
   * @param maxLength - Specifies the maximum length of the text.
   * @return the component itself.
   */
  @Override
  public TextArea setMaxLength(int maxLength) {
    if (maxLength < 0) {
      throw new IllegalArgumentException("Max length must be greater than or equal to 0");
    }

    this.maxLength = maxLength;
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setMaxLength(maxLength);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the maximum length of the text.
   *
   * @return the maximum length of the text.
   */
  @Override
  public int getMaxLength() {
    return maxLength;
  }

  /**
   * Sets the minimum length of the text.
   *
   * @param minLength - Specifies the minimum length of the text.
   * @return the component itself.
   */
  @Override
  public TextArea setMinLength(int minLength) {
    if (minLength < 0) {
      throw new IllegalArgumentException("Min length must be greater than or equal to 0");
    }

    this.minLength = minLength;
    setUnrestrictedProperty("minLength", minLength);

    return this;
  }

  /**
   * Gets the minimum length of the text.
   *
   * @return the minimum length of the text.
   */
  @Override
  public int getMinLength() {
    return minLength;
  }

  /**
   * Sets the number of visible text lines for the component.
   *
   * @param rows - Specifies the number of rows.
   * @return the component itself.
   */
  public TextArea setRows(int rows) {
    if (rows < 0) {
      throw new IllegalArgumentException("Rows must be greater than or equal to 0");
    }

    this.rows = rows;
    setUnrestrictedProperty("rows", rows);

    return this;
  }

  /**
   * Gets the number of visible text lines for the component.
   *
   * @return the number of rows.
   */
  public int getRows() {
    return rows;
  }

  /**
   * Sets The visible width of the component, in average character widths. If it is specified, it
   * must be a positive integer. If it is not specified, the default value is 20.
   *
   * @param columns - Specifies the number of columns.
   * @return the component itself.
   */
  public TextArea setColumns(int columns) {
    if (columns < 0) {
      throw new IllegalArgumentException("columns must be greater than or equal to 0");
    }

    this.columns = columns;
    setUnrestrictedProperty("cols", columns);

    return this;
  }

  /**
   * Gets the number of columns in the text area.
   *
   * @return the number of columns.
   * @see #setColumns(int)
   */
  public int getColumns() {
    return columns;
  }

  /**
   * Sets whether the text area should wrap text.
   *
   * @param lineWrap - Specifies whether the text area should wrap text.
   * @return the component itself.
   */
  public TextArea setLineWrap(boolean lineWrap) {
    this.lineWrap = lineWrap;

    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setLineWrap(lineWrap);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the text area wraps text.
   *
   * @return whether the text area wraps text.
   */
  public boolean isLineWrap() {
    return lineWrap;
  }

  /**
   * Sets whether the text area should have a horizontal scroll bar.
   *
   * @param horizontalScroll - Specifies whether the text area should have a horizontal scroll bar.
   * @return the component itself.
   */
  public TextArea setHorizontalScroll(boolean horizontalScroll) {
    this.horizontalScroll = horizontalScroll;

    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setHorizontalScrollable(horizontalScroll);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the text area has a horizontal scroll bar.
   *
   * @return whether the text area has a horizontal scroll bar.
   */
  public boolean isHorizontalScroll() {
    return horizontalScroll;
  }

  /**
   * Sets whether the text area should have a vertical scroll bar.
   *
   * @param verticalScroll - Specifies whether the text area should have a vertical scroll bar.
   * @return the component itself.
   */
  public TextArea setVerticalScroll(boolean verticalScroll) {
    this.verticalScroll = verticalScroll;

    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setVerticalScrollable(verticalScroll);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets whether the text area has a vertical scroll bar.
   *
   * @return whether the text area has a vertical scroll bar.
   */
  public boolean isVerticalScroll() {
    return verticalScroll;
  }

  /**
   * Sets the style of wrapping used if the text area is wrapping lines.
   *
   * @param wrapStyle - Specifies the style of wrapping.
   * @return the component itself.
   */
  public TextArea setWrapStyle(WrapStyle wrapStyle) {
    this.wrapStyle = wrapStyle;
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setWrapStyleWord(wrapStyle == WrapStyle.WORD_BOUNDARIES);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * Gets the style of wrapping used if the text area is wrapping lines.
   *
   * @return the style of wrapping.
   */
  public WrapStyle getWrapStyle() {
    return wrapStyle;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TextArea setTypingMode(TypingMode typingMode) {
    this.typingMode = typingMode;
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.setOvertypeMode(typingMode == TypingMode.OVERWRITE);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TypingMode getTypingMode() {
    return typingMode;
  }

  /**
   * Selects a range of text in the TextArea.
   *
   * @param range the range to select.
   * @return the component itself.
   */
  public TextArea setSelectionRange(SelectionRange range) {
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        field.highlight(range.getStartParagraph(), range.getStartOffset(), range.getEndParagraph(),
            range.getEndOffset());
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.range = range;
    return this;
  }

  /**
   * Selects a range of text in the TextArea given a starting and ending position.
   *
   * @param start the start of the selection range.
   * @param end the end of the selection range.
   *
   * @return the component itself.
   */
  public TextArea setSelectionRange(int start, int end) {
    return setSelectionRange(new SelectionRange(start, end));
  }

  /**
   * Gets the selection range.
   *
   * @return the selection range.
   */
  public SelectionRange getSelectionRange() {
    BBjCEdit field = inferField();

    if (field != null) {
      try {
        BBjVector selection = field.getSelection();
        return new SelectionRange((int) selection.get(0), (int) selection.get(1),
            (int) selection.get(2), (int) selection.get(3));
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    if (this.range == null) {
      return new SelectionRange(0, 0);
    }

    return this.range;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> getRestrictedProperties() {
    List<String> properties = super.getRestrictedProperties();
    properties.addAll(Arrays.asList("autoValidate", "autoValidateOnLoad", "autoWasValidated",
        "autocomplete", "autocorrect", "autofocus", "cols", "disabled", "expanse", "hasFocus",
        "highlightBehaviors", "insertMode", "invalid", "invalidMessage", "label", "maxLineCount",
        "maxLineLength", "maxlength", "minlength", "name", "placeholder", "readonly", "required",
        "rows", "spellcheck", "tabTraversable", "valid"));

    return properties;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onCreate(Window window) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(window);
      byte[] flags = BBjFunctionalityHelper.buildStandardCreationFlags(isVisible(), isEnabled());
      setControl(w.addCEdit(getText(), flags));
    } catch (BBjException | IllegalAccessException e) {
      throw new WebforjRuntimeException("Failed to create BBjCEdit", e);
    }
  }

  @Override
  protected void onAttach() {
    super.onAttach();

    if (!paragraphs.isEmpty()) {
      addParagraphs(paragraphs);
    }

    if (lineCountLimit != Integer.MAX_VALUE) {
      setLineCountLimit(lineCountLimit);
    }

    if (paragraphLengthLimit != Integer.MAX_VALUE) {
      setParagraphLengthLimit(paragraphLengthLimit);
    }

    if (maxLength != Integer.MAX_VALUE) {
      setMaxLength(maxLength);
    }

    if (Boolean.TRUE.equals(lineWrap)) {
      setLineWrap(lineWrap);
    }

    if (Boolean.TRUE.equals(horizontalScroll)) {
      setHorizontalScroll(horizontalScroll);
    }

    if (Boolean.TRUE.equals(verticalScroll)) {
      setVerticalScroll(verticalScroll);
    }

    if (wrapStyle != WrapStyle.WORD_BOUNDARIES) {
      setWrapStyle(wrapStyle);
    }

    if (typingMode != TypingMode.INSERT) {
      setTypingMode(typingMode);
    }

    if (this.range != null) {
      setSelectionRange(this.range);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String convertValue(String value) {
    return value;
  }

  /**
   * Gets the instance of the underlying BBjEditBox control.
   *
   * @return the instance of the control
   */
  private BBjCEdit inferField() {
    try {
      return (BBjCEdit) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
