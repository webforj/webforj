package org.dwcj.component.event.sink;

import com.basis.bbj.proxies.event.BBjEvent;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.event.EventDispatcher;

/** Mock for the AbstractSink. */
public class AbstractSinkMock extends AbstractSink {

  public AbstractSinkMock() {
    super(null, null, 0);
  }

  protected AbstractSinkMock(AbstractDwcComponent component, EventDispatcher dispatcher,
      int eventType) {
    super(component, dispatcher, eventType);
  }

  @Override
  public void handleEvent(BBjEvent ev) {}

}
