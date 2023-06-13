package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;

/** Mock for the AbstractSink. */
public class SinkMock extends AbstractSink {

  public SinkMock() {
    super(null, new EventDispatcher(), 0);
  }

  protected SinkMock(AbstractDwcComponent component, EventDispatcher dispatcher, int eventType) {
    super(component, dispatcher, eventType);
  }

  @Override
  public void handleEvent(BBjEvent ev) {}
}
