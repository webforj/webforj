package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.concern.HasHighlightOnFocus;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasMaxLength;
import com.webforj.concern.HasMinLength;
import com.webforj.concern.HasPattern;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.data.selection.SelectionRange;
import com.webforj.dispatcher.EventListener;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * The Base class for all DWC none masked text fields components.
 *
 * <p>
 * This abstract class serves as the foundation for all text field components within the framework.
 * It extends the {@link DwcField} class and implements several interfaces for handling text
 * field-specific properties and behaviors.
 * </p>
 *
 * @param <T> The type of the component.
 *
 * @see DwcField
 * @see HasMinLength
 * @see HasMaxLength
 * @see HasHighlightOnFocus
 * @see HasHorizontalAlignment
 * @see SelectionRange
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
// We're purposefully ignoring the deep inheritance warning here because we've designed our class
// hierarchy to meet the unique requirements of our UI framework. This design closely aligns with
// our framework's specific goals and emphasizes the need for caution when considering any changes.
//
// Any changes to the inheritance structure should be thoughtfully evaluated in the context of our
// framework's needs. The current structure is essential for meeting those needs.
@SuppressWarnings("squid:S110")
abstract class DwcTextField<T extends DwcTextField<T>> extends DwcFieldInitializer<T, String>
    implements HasMinLength<T>, HasMaxLength<T>, HasHorizontalAlignment<T>, HasPattern<T> {

  private int minLength = 0;
  private int maxLength = 524288;
  private SelectionRange range = null;
  private String pattern = null;
  private String autocomplete = "off";

  /**
   * Constructs a new field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  DwcTextField(String label, String value, String placeholder) {
    super(label, value, placeholder);
    postInit();
  }

  /**
   * Constructs a new field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  DwcTextField(String label, String value, EventListener<ValueChangeEvent<String>> listener) {
    super(label, value, listener);
    postInit();
  }

  /**
   * Constructs a new field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  DwcTextField(String label, String value) {
    super(label, value);
    postInit();
  }

  /**
   * Constructs a new field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  DwcTextField(String label, EventListener<ValueChangeEvent<String>> listener) {
    super(label, listener);
    postInit();
  }

  /**
   * Constructs a new field with a value change listener.
   *
   * @param listener the value change listener
   */
  DwcTextField(EventListener<ValueChangeEvent<String>> listener) {
    super(listener);
    postInit();
  }

  /**
   * Constructs a new field with a label.
   *
   * @param label the label of the field
   */
  DwcTextField(String label) {
    super(label);
    postInit();
  }

  /**
   * Constructs a new field.
   */
  DwcTextField() {
    super();
    postInit();
  }

  private void postInit() {
    setComponentDefaultHorizontalAlignment(Alignment.LEFT);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return getText();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setValue(String value) {
    setText(value);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMaxLength(int maxLength) {
    this.maxLength = maxLength;
    setUnrestrictedProperty("maxlength", maxLength);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getMaxLength() {
    return maxLength;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMinLength(int minLength) {
    this.minLength = minLength;
    setUnrestrictedProperty("minlength", minLength);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getMinLength() {
    return minLength;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setPattern(String pattern) {
    this.pattern = pattern;
    setUnrestrictedProperty("pattern", pattern);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getPattern() {
    return pattern;
  }

  /**
   * Specify what if any permission the user agent has to provide automated assistance in filling
   * out form field values, as well as guidance to the browser as to the type of information
   * expected in the field.
   *
   * <p>
   * The value is either the keyword <code>off</code> or <code>on</code>, or a space-separated <a
   * href=https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete#values>token-list</a>
   * that describes the meaning of the autocompletion value.
   *
   * For example, a value of "off" means that the browser should not automatically complete the
   * input. A value of "username" means that the browser should automatically complete the input
   * with the user's username.
   * </p>
   *
   * @param autocomplete the autocomplete value
   * @return the component itself
   */
  public T setAutoComplete(String autocomplete) {
    this.autocomplete = autocomplete;
    setUnrestrictedProperty("autocomplete", this.autocomplete);

    return getSelf();
  }

  /**
   * Get the autocomplete value.
   *
   * @return the autocomplete value
   */
  public String getAutoComplete() {
    return this.autocomplete;
  }

  /**
   * Selects a range of text in the input field.
   *
   * @param range the range to select.
   * @return the text field instance.
   */
  public T setSelectionRange(SelectionRange range) {
    BBjEditBox editbox = inferField();

    if (editbox != null) {
      editbox.select(range.getStart(), range.getEnd());
    }

    this.range = range;
    return getSelf();
  }

  /**
   * Selects a range of text in the input field given a starting and ending position.
   *
   * @param start the start of the selection range.
   * @param end the end of the selection range.
   *
   * @return the text field instance.
   */
  public T setSelectionRange(int start, int end) {
    return setSelectionRange(new SelectionRange(start, end));
  }

  /**
   * Gets the selection range.
   *
   * @return the selection range.
   */
  public SelectionRange getSelectionRange() {
    BBjEditBox editbox = inferField();

    if (editbox != null) {
      try {
        BBjVector selection = editbox.getSelection();
        return new SelectionRange((int) selection.get(1), (int) selection.get(3));
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
   * Gets the selected text in the input field.
   *
   * @return the selected text.
   */
  public String getSelectedText() {
    BBjEditBox editbox = inferField();

    if (editbox != null) {
      try {
        return editbox.getSelectedText();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    SelectionRange selectedRange = getSelectionRange();
    if (!selectedRange.isEmpty()) {
      return getValue().substring(selectedRange.getStart(), selectedRange.getEnd());
    }

    return "";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHorizontalAlignment(Alignment alignment) {
    setComponentHorizontalAlignment(alignment);
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public Alignment getHorizontalAlignment() {
    return getComponentHorizontalAlignment();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void onAttach() {
    super.onAttach();

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
}
