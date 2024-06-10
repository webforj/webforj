package com.webforj.component.field;

import com.basis.bbj.proxies.sysgui.BBjInput;
import com.basis.startup.type.BBjException;
import com.webforj.annotation.ExcludeFromJacocoGeneratedReport;
import com.webforj.bridge.ComponentAccessor;
import com.webforj.concern.HasHorizontalAlignment;
import com.webforj.concern.HasMask;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasRestoreValue;
import com.webforj.concern.HasTypingMode;
import com.webforj.data.selection.SelectionRange;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Objects;

/**
 * Represents a masked field.
 *
 * @param <T> The type of the component.
 * @param <V> The type of value associated with the field.
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
public abstract class DwcMaskedField<T extends DwcField<T, V> & HasReadOnly<T>, V>
    extends DwcField<T, V>
    implements HasMask<T>, HasTypingMode<T>, HasHorizontalAlignment<T>, HasRestoreValue<T, V> {
  private String mask = "";
  private int caretPosition = 0;
  private TypingMode typingMode = TypingMode.OVERWRITE;
  private SelectionRange range = null;
  private V restoreValue = null;

  /**
   * {@inheritDoc}
   */
  @Override
  public T setMask(String mask) {
    Objects.requireNonNull(mask, "The mask cannot be null");

    this.mask = mask;
    BBjInput field = inferField();

    if (field != null) {
      try {
        field.setMask(mask);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getMask() {
    return this.mask;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setRestoreValue(V value) {
    BBjInput field = inferField();

    if (field != null) {
      try {
        field.setRestore(String.valueOf(value));
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    this.restoreValue = value;
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public V getRestoreValue() {
    return restoreValue;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T restoreValue() {
    return setValue(restoreValue);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public T setTypingMode(TypingMode typingMode) {
    this.typingMode = typingMode;
    BBjInput field = inferField();

    if (field != null) {
      try {
        field.setInsertMode(typingMode == TypingMode.OVERWRITE);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public TypingMode getTypingMode() {
    return typingMode;
  }

  /**
   * Sets the caret position for the field.
   *
   * @param caretPosition the caret position to set
   * @return the instance of the field
   */
  public T setCaretPosition(int caretPosition) {
    this.caretPosition = caretPosition;
    BBjInput field = inferField();

    if (field != null) {
      try {
        field.setCaretPosition(caretPosition);
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return getSelf();
  }

  /**
   * Gets the caret position for the field.
   *
   * @return the caret position
   */
  public int getCaretPosition() {
    BBjInput field = inferField();

    if (field != null) {
      try {
        this.caretPosition = field.getCaretPosition();
      } catch (BBjException e) {
        throw new WebforjRuntimeException(e);
      }
    }

    return this.caretPosition;
  }

  /**
   * Selects a range of text in the input field.
   *
   * @param range the range to select.
   * @return the component itself.
   */
  public T setSelectionRange(SelectionRange range) {
    BBjInput field = inferField();

    if (field != null) {
      field.select(range.getStart(), range.getEnd());
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
   * @return the component itself.
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
    SelectionRange selectedRange = getSelectionRange();
    String text = getText();
    text = text == null ? "" : text;

    if (selectedRange.isEmpty()) {
      return "";
    }

    if (selectedRange.getStart() < 0 || selectedRange.getEnd() > text.length()) {
      return "";
    }

    return text.substring(selectedRange.getStart(), selectedRange.getEnd());
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
    setMask(this.mask);

    if (this.caretPosition > 0) {
      setCaretPosition(this.caretPosition);
    }

    if (typingMode != TypingMode.OVERWRITE) {
      setTypingMode(typingMode);
    }

    if (this.range != null) {
      setSelectionRange(this.range);
    }

    if (restoreValue != null) {
      setRestoreValue(restoreValue);
    }
  }

  /**
   * Gets the instance of the underlying BBjInput control.
   *
   * @return the instance of the control
   */
  protected BBjInput inferField() {
    try {
      return (BBjInput) ComponentAccessor.getDefault().getControl(this);
    } catch (IllegalAccessException e) {
      throw new WebforjRuntimeException(e);
    }
  }
}
