package com.webforj.component.event.sink;

import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.webforj.component.DwcComponent;
import com.webforj.dispatcher.EventDispatcher;

/** Mock for the AbstractSink. */
public class EventSinkMock extends AbstractDwcEventSink {
  private BBjControl control;

  public EventSinkMock() {
    this(null, new EventDispatcher(), 0);
  }

  public EventSinkMock(DwcComponent<?> component, EventDispatcher dispatcher, int eventType) {
    super(component, dispatcher, eventType);
    control = mock(BBjControl.class);
  }

  @Override
  public void handleEvent(BBjEvent ev) {
    // no-op
  }

  @Override
  protected BBjControl getControl() {
    return control;
  }
}
