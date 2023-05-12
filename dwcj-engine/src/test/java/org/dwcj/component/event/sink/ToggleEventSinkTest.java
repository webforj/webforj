package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.ToggleEvent;
import org.dwcj.mocks.BBjCheckChangeEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the CheckChangedEvent.
 */
class ToggleEventSinkTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjCheckChangeEventMock eventMock;
  ToggleEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjCheckChangeEventMock();
  }

  @Test
  @DisplayName("Test the CheckChangedEvent payload")
  void payload() {
    ToggleEventSink sink = new ToggleEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(ToggleEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.isChecked(), dispatchedEvent.isToggled());

  }
}
