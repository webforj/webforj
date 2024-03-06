package com.webforj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.DwcComponentMock;
import com.webforj.component.event.mocks.BBjKeypressEventMock;
import com.webforj.component.event.sink.KeypressEventSink;
import com.webforj.dispatcher.EventDispatcher;
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
    dispatcher.addListener(KeypressEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getKeyCode(), dispatchedEvent.getKeyCode().getValue());
    assertEquals(eventMock.isAltDown(), dispatchedEvent.isAltKey());
    assertEquals(eventMock.isCmdDown(), dispatchedEvent.isCmdKey());
    assertEquals(eventMock.isControlDown(), dispatchedEvent.isControlKey());
    assertEquals(eventMock.isShiftDown(), dispatchedEvent.isShiftKey());
  }
}


