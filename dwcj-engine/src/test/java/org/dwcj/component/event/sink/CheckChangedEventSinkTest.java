package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.CheckChangedEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.mocks.BBjCheckChangeEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the CheckChangedEvent.
 */
class CheckChangedEventSinkTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjCheckChangeEventMock eventMock;
  CheckChangedEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjCheckChangeEventMock();
  }

  @Test
  @DisplayName("Test the CheckChangedEvent payload")
  void payload() {
    CheckChangedEventSink sink = new CheckChangedEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(CheckChangedEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.isChecked(), dispatchedEvent.isChecked());

  }
}
