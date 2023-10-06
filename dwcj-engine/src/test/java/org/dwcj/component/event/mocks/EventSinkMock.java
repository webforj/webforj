package org.dwcj.component.event.mocks;

import com.basis.bbj.proxies.event.BBjEvent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.sink.AbstractDwcEventSink;

/** Mock for the AbstractSink. */
public class EventSinkMock extends AbstractDwcEventSink {

  public EventSinkMock() {
    super(null, new EventDispatcher(), 0);
  }

  protected EventSinkMock(AbstractDwcComponent component, EventDispatcher dispatcher,
      int eventType) {
    super(component, dispatcher, eventType);
  }

  @Override
  public void handleEvent(BBjEvent ev) {}
}
