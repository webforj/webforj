package com.webforj.component.field;

import com.webforj.concern.HasLocale;
import com.webforj.concern.HasMax;
import com.webforj.concern.HasMin;
import com.webforj.concern.HasPattern;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import java.util.Locale;

/**
 * Base class for masked date & time fields.
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
abstract class DwcDateTimeMaskedField<T extends DwcMaskedField<T, V>, V> extends
    DwcMaskedField<T, V> implements HasPattern<T>, HasLocale<T>, HasMin<T, V>, HasMax<T, V> {
  private Locale locale;
  private String pattern;
  private boolean allowCustomValue = true;

  /**
   * Constructs a new masked field with a label, value, and placeholder.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param placeholder the placeholder of the field
   */
  DwcDateTimeMaskedField(String label, V value, String placeholder) {
    super(label, value, placeholder);
  }

  /**
   * Constructs a new masked field with a label, value, and a value change listener.
   *
   * @param label the label of the field
   * @param value the value of the field
   * @param listener the value change listener
   */
  DwcDateTimeMaskedField(String label, V value, EventListener<ValueChangeEvent<V>> listener) {
    super(label, value, listener);
  }

  /**
   * Constructs a new masked field with a label and value.
   *
   * @param label the label of the field
   * @param value the value of the field
   */
  DwcDateTimeMaskedField(String label, V value) {
    super(label, value);
  }

  /**
   * Constructs a new masked field with a label and a value change listener.
   *
   * @param label the label of the field
   * @param listener the value change listener
   */
  DwcDateTimeMaskedField(String label, EventListener<ValueChangeEvent<V>> listener) {
    super(label, listener);
  }

  /**
   * Constructs a new masked field with a value change listener.
   *
   * @param listener the value change listener
   */
  DwcDateTimeMaskedField(EventListener<ValueChangeEvent<V>> listener) {
    super(listener);
  }

  /**
   * Constructs a new masked field with a label.
   *
   * @param label the label of the field
   */
  DwcDateTimeMaskedField(String label) {
    super(label);
  }

  /**
   * Constructs a new masked field.
   */
  DwcDateTimeMaskedField() {
    super();
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
   * {@inheritDoc}
   */
  @Override
  public T setLocale(Locale locale) {
    this.locale = locale;
    setUnrestrictedProperty("locale", locale.toString());
    return getSelf();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Locale getLocale() {
    return locale;
  }

  /**
   * When false, the user won't be able to change the input's text.
   *
   * @param allowCustomValue true to allow custom value, false otherwise
   * @return the component itself
   */
  public T setAllowCustomValue(boolean allowCustomValue) {
    this.allowCustomValue = allowCustomValue;
    setUnrestrictedProperty("customValue", allowCustomValue);
    return getSelf();
  }

  /**
   * Checks whether the user is allowed to enter custom value.
   *
   * @return true if the user is allowed to enter custom value, false otherwise
   */
  public boolean isAllowCustomValue() {
    return allowCustomValue;
  }
}
