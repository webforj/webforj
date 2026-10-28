package com.webforj.automation.act;

import com.webforj.component.element.ElementComposite;
import com.webforj.component.element.annotation.NodeName;
import com.webforj.concern.HasReadOnly;
import com.webforj.concern.HasValue;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

/**
 * Value holding composite used to test components that bring their own contract.
 */
@NodeName("x-value")
public final class CustomValue<V> extends ElementComposite
    implements HasValue<CustomValue<V>, V>, HasReadOnly<CustomValue<V>> {
  private V value;
  private boolean readOnly;

  /**
   * Creates a value holder with the given initial value.
   *
   * @param value the initial value
   */
  public CustomValue(V value) {
    this.value = value;
  }

  @Override
  public V getValue() {
    return value;
  }

  @Override
  public CustomValue<V> setValue(V value) {
    this.value = value;
    getEventDispatcher().dispatchEvent(new ValueChangeEvent<>(this, value));

    return this;
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<V>> addValueChangeListener(
      EventListener<ValueChangeEvent<V>> listener) {
    return getEventDispatcher().addListener(ValueChangeEvent.class, listener);
  }

  @Override
  public CustomValue<V> setReadOnly(boolean readOnly) {
    this.readOnly = readOnly;

    return this;
  }

  @Override
  public boolean isReadOnly() {
    return readOnly;
  }
}
