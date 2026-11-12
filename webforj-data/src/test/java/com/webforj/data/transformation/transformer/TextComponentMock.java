package com.webforj.data.transformation.transformer;

import com.webforj.data.concern.ValueAware;
import com.webforj.data.event.ValueChangeEvent;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;

class TextComponentMock implements ValueAware<TextComponentMock, String> {
  private final EventDispatcher dispatcher = new EventDispatcher();
  private String value = "";

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public TextComponentMock setValue(String value) {
    this.value = value;
    dispatcher.dispatchEvent(new ValueChangeEvent<>(this, value));
    return this;
  }

  @Override
  public ListenerRegistration<ValueChangeEvent<String>> addValueChangeListener(
      EventListener<ValueChangeEvent<String>> listener) {
    return dispatcher.addListener(ValueChangeEvent.class, listener);
  }
}
