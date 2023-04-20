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
  static DwcComponentMock mockComponent = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjMouseEventMock mockEvent;
  MouseEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    mockEvent = new BBjMouseEventMock();
  }

  /**
   * A method returning the sinks for the parameterized test.
   *
   * @return An array of {@link AbstractMouseEventSink}
   */
  public static AbstractMouseEventSink[] data() {
    return new AbstractMouseEventSink[] {new MouseEnterEventSink(mockComponent, dispatcher),
        new MouseExitEventSink(mockComponent, dispatcher),
        new RightMouseDownEventSink(mockComponent, dispatcher)};
  }

  @ParameterizedTest
  @MethodSource(value = "data")
  @DisplayName("MouseEvent tests")
  void testMouseEnter(AbstractMouseEventSink sink) {
    EventListener<MouseEvent> listener = this::testMethod;
    dispatcher.addEventListener(MouseEvent.class, listener);

    sink.handleEvent(mockEvent);
    assertEquals(2, dispatchedEvent.getMouseButton());
    assertEquals(10, dispatchedEvent.getScreenX());
    assertEquals(20, dispatchedEvent.getScreenY());
    assertEquals(30, dispatchedEvent.getX());
    assertEquals(40, dispatchedEvent.getY());
    assertFalse(dispatchedEvent.isAltDown());
    assertFalse(dispatchedEvent.isCmdDown());
    assertFalse(dispatchedEvent.isControlDown());
    assertTrue(dispatchedEvent.isShiftDown());
  }

  void testMethod(MouseEvent ev) {
    dispatchedEvent = ev;
  }

}


