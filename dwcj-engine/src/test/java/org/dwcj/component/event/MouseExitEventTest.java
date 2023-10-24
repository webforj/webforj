package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.DwcComponentMock;
import org.dwcj.component.event.mocks.BBjMouseEventMock;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the MouseExitEvent.
 */
class MouseExitEventTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjMouseEventMock eventMock;
  MouseEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjMouseEventMock();
  }

  @Test
  @DisplayName("Test the MouseExitEvent payload")
  void payload() {
    MouseExitEventSink sink = new MouseExitEventSink(componentMock, dispatcher);
    dispatcher.addListener(MouseExitEvent.class, e -> dispatchedEvent = e);
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


