package org.dwcj.component.event.sink;

import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.component.DwcComponent;
import org.dwcj.component.DwcHelperMock;
import org.dwcj.dispatcher.EventDispatcher;

/** Mock for the AbstractSink. */
public class EventSinkMock extends AbstractDwcEventSink {
  private BBjControl control;

  public EventSinkMock() {
    this(null, new EventDispatcher(), 0);
  }

  public EventSinkMock(DwcComponent<?> component, EventDispatcher dispatcher, int eventType) {
    super(component, dispatcher, eventType);
    control = mock(BBjControl.class);
    setDwcjHelper(new DwcHelperMock());
  }

  @Override
  public void handleEvent(BBjEvent ev) {}

  @Override
  protected BBjControl getControl() {
    return control;
  }
}
