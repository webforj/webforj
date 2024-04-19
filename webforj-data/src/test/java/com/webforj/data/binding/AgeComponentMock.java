package com.webforj.data.binding;

import com.webforj.data.concern.ValueAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

class AgeComponentMock implements ValueAware<AgeComponentMock, Integer> {
  private EventDispatcher dispatcher = new EventDispatcher();
  private Integer value = 0;

  public AgeComponentMock(Integer value) {
    setValue(value);
  }

  public AgeComponentMock() {
    this(0);
  }

  @Override
  public Integer getValue() {
    return value;
  }

  @Override
  public AgeComponentMock setValue(Integer value) {
    this.value = value;
    dispatcher.dispatchEvent(new ValueChangeEvent<>(this, value));
    return this;
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<Integer>> addValueChangeListener(
      EventListener<ValueChangeEvent<Integer>> listener) {
    return dispatcher.addListener(ValueChangeEvent.class, listener);
  }
}
