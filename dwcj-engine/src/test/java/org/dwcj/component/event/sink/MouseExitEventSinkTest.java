package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.MouseEvent;
import org.dwcj.component.event.MouseExitEvent;
import org.dwcj.mocks.BBjMouseEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the MouseExitEvent.
 */
public class MouseExitEventSinkTest {
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
    dispatcher.addEventListener(MouseExitEvent.class, e -> dispatchedEvent = e);
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


