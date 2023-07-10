package org.dwcj.component.field;

import com.basis.bbj.proxies.sysgui.BBjEditBox;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.annotation.ExcludeFromJacocoGeneratedReport;
import org.dwcj.component.HasMaxLength;
import org.dwcj.component.HasMinLength;
import org.dwcj.component.HighlightableOnFocus;
import org.dwcj.component.HorizontalAlignment;
import org.dwcj.component.SelectionRange;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * The Base class for all fields components.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
abstract class AbstractTextField<T extends AbstractTextField<T>> extends AbstractField<T, String>
    implements HasMinLength<T>, HasMaxLength<T>, HighlightableOnFocus<T>, HorizontalAlignment<T> {

  private int minLength = 0;
  private int maxLength = 524288;
  private SelectionRange range = null;

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
   * Select a range of text in the input field.
   *
   * @param range the range to select.
   * @return the text field instance.
   */
  public T setSelectionRange(SelectionRange range) {
    BBjEditBox editbox = getBbjControl();

    if (editbox != null) {
      editbox.select(range.getStart(), range.getEnd());
    }

    this.range = range;
    return getSelf();
  }

  /**
   * Select a range of text in the input field.
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
    BBjEditBox editbox = getBbjControl();

    if (editbox != null) {
      try {
        BBjVector selection = editbox.getSelection();
        return new SelectionRange((int) selection.get(1), (int) selection.get(3));
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
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
    BBjEditBox editbox = getBbjControl();

    if (editbox != null) {
      try {
        return editbox.getSelectedText();
      } catch (BBjException e) {
        throw new DwcjRuntimeException(e);
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
  public Behavior getHighlightOnFocus() {
    return getComponentHighlightOnFocus();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @ExcludeFromJacocoGeneratedReport
  public T setHighlightOnFocus(Behavior highlight) {
    setComponentHighlightOnFocus(highlight);
    return getSelf();
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
  protected void catchUp() throws IllegalAccessException {
    super.catchUp();

    if (this.range != null) {
      setSelectionRange(this.range);
    }
  }
}
