package org.dwcj.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEvent;
import org.dwcj.component.event.sink.AbstractMouseEventSink;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.event.sink.MouseExitEventSink;
import org.dwcj.component.event.sink.RightMouseDownEventSink;
import org.dwcj.mocks.BBjMouseEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * A class for testing the MouseEvent with different sinks.
 */
public class MouseEventTests {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjMouseEventMock eventMock;
  MouseEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjMouseEventMock();
  }

  /**
   * A method returning the sinks for the parameterized test.
   *
   * @return An array of {@link AbstractMouseEventSink}
   */
  public static AbstractMouseEventSink[] getMouseEventSinks() {
    return new AbstractMouseEventSink[] {new MouseEnterEventSink(componentMock, dispatcher),
        new MouseExitEventSink(componentMock, dispatcher),
        new RightMouseDownEventSink(componentMock, dispatcher)};
  }

  @ParameterizedTest
  @MethodSource(value = "getMouseEventSinks")
  @DisplayName("Test the MouseEvent payload with different sinks")
  void payload(AbstractMouseEventSink sink) {
    dispatcher.addEventListener(MouseEvent.class, e -> dispatchedEvent = e);
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


