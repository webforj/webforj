package com.webforj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.component.DwcComponentMock;
import com.webforj.component.event.mocks.BBjCheckChangeEventMock;
import com.webforj.component.event.sink.ToggleEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the ToggleEvent.
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
  @DisplayName("Test the ToggleEvent payload")
  void payload() {
    ToggleEventSink sink = new ToggleEventSink(componentMock, dispatcher);
    dispatcher.addListener(ToggleEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.isChecked(), dispatchedEvent.isToggled());

  }
}
