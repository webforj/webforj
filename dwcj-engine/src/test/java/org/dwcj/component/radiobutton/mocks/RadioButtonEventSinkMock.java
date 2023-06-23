package org.dwcj.component.radiobutton.mocks;

import com.basis.bbj.proxies.event.BBjEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.radiobutton.RadioButtonGroup;
import org.dwcj.component.radiobutton.sink.AbstractRadioButtonEventSink;

public class RadioButtonEventSinkMock extends AbstractRadioButtonEventSink {

  public RadioButtonEventSinkMock() {
    super(null, new EventDispatcher(), 0);
  }

  protected RadioButtonEventSinkMock(RadioButtonGroup component, EventDispatcher dispatcher,
      int eventType) {
    super(component, dispatcher, eventType);
  }

  @Override
  public void handleEvent(BBjEvent ev) {}
}
