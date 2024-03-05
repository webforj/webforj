package com.webforj.component.window.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.event.BBjClickEvent;
import com.webforj.component.window.Window;
import com.webforj.component.window.event.WindowClickEvent;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the MouseEnterEvent.
 */
class ClickEventSinkTest {

  WindowClickEvent dispatchedEvent;

  @Test
  void payload() {
    EventDispatcher dispatcher = new EventDispatcher();
    Window windowMock = mock(Window.class);
    BBjClickEvent eventMock = mock(BBjClickEvent.class);

    WindowClickEventSink sink = new WindowClickEventSink(windowMock, dispatcher);
    dispatcher.addListener(WindowClickEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getClickCount(), dispatchedEvent.getClickCount());

    assertEquals(eventMock.getMouseButton(), dispatchedEvent.getMouseButton());
    assertEquals(eventMock.getScreenX(), dispatchedEvent.getScreenX());
    assertEquals(eventMock.getScreenY(), dispatchedEvent.getScreenY());
    assertEquals(eventMock.getX(), dispatchedEvent.getX());
    assertEquals(eventMock.getY(), dispatchedEvent.getY());

    assertEquals(eventMock.isAltDown(), dispatchedEvent.isAltDown());
    assertEquals(eventMock.isCmdDown(), dispatchedEvent.isCmdDown());
    assertEquals(eventMock.isControlDown(), dispatchedEvent.isControlDown());
    assertEquals(eventMock.isShiftDown(), dispatchedEvent.isShiftDown());
  }
}


