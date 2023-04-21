package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.EditModifyEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.mocks.BBjEditModifyEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the EditModifyEvent.
 */
class EditModifyEventSinkTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjEditModifyEventMock eventMock;
  EditModifyEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjEditModifyEventMock();
  }

  @Test
  @DisplayName("Test the EditModifyEvent payload")
  void payload() {
    EditModifyEventSink sink = new EditModifyEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(EditModifyEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getText(), dispatchedEvent.getText());
  }
}
