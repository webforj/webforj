package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.mocks.BBjKeypressEventMock;
import org.dwcj.component.event.sink.KeypressEventSink;
import org.dwcj.component.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class KeypressEventTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjKeypressEventMock eventMock;
  KeypressEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjKeypressEventMock();
  }

  @Test
  @DisplayName("Test the KeypressEvent payload")
  void payload() {
    KeypressEventSink sink = new KeypressEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(KeypressEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getKeyCode(), dispatchedEvent.getKeyCode().getValue());
    assertEquals(eventMock.isAltDown(), dispatchedEvent.isAltKey());
    assertEquals(eventMock.isCmdDown(), dispatchedEvent.isCmdKey());
    assertEquals(eventMock.isControlDown(), dispatchedEvent.isControlKey());
    assertEquals(eventMock.isShiftDown(), dispatchedEvent.isShiftKey());
  }
}


