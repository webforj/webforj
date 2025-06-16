package com.webforj.component.optioninput.mocks;

import com.basis.bbj.proxies.event.BBjEvent;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.optioninput.sink.AbstractRadioButtonEventSink;
import com.webforj.dispatcher.EventDispatcher;

public class RadioButtonEventSinkMock extends AbstractRadioButtonEventSink {

  public RadioButtonEventSinkMock() {
    super(null, new EventDispatcher(), 0);
  }

  protected RadioButtonEventSinkMock(RadioButtonGroup component, EventDispatcher dispatcher,
      int eventType) {
    super(component, dispatcher, eventType);
  }

  @Override
  public void handleEvent(BBjEvent ev) {
    // no-op
  }
}
