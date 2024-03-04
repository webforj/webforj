package com.webforj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.DwcComponentMock;
import com.webforj.component.event.mocks.BBjMouseEventMock;
import com.webforj.component.event.sink.MouseEnterEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the MouseEnterEvent.
 */
class MouseEnterEventTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjMouseEventMock eventMock;
  MouseEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjMouseEventMock();
  }

  @Test
  @DisplayName("Test the MouseEnterEvent payload")
  void payload() {
    MouseEnterEventSink sink = new MouseEnterEventSink(componentMock, dispatcher);
    dispatcher.addListener(MouseEnterEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

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


