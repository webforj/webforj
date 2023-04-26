package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.ModifyEvent;
import org.dwcj.mocks.BBjEditModifyEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the EditModifyEvent.
 */
class ModifyEventSinkTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjEditModifyEventMock eventMock;
  ModifyEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjEditModifyEventMock();
  }

  @Test
  @DisplayName("Test the ModifyEvent payload")
  void payload() {
    ModifyEventSink sink = new ModifyEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(ModifyEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getText(), dispatchedEvent.getText());
  }
}
