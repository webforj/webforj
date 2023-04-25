package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.dwcj.component.event.EditKeypressEvent;
import org.dwcj.component.event.EventDispatcher;
import org.dwcj.mocks.BBjKeypressEventMock;
import org.dwcj.mocks.DwcComponentMock;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * A class for testing the EditKeypressEvent.
 */
class EditKeypressEventSinkTest {
  static DwcComponentMock componentMock = new DwcComponentMock();
  static EventDispatcher dispatcher = new EventDispatcher();
  BBjKeypressEventMock eventMock;
  EditKeypressEvent dispatchedEvent;

  @BeforeEach
  void setUp() {
    eventMock = new BBjKeypressEventMock();
  }

  @Test
  @DisplayName("Test the EditKeypressEvent payload")
  void payload() {
    EditKeypressEventSink sink = new EditKeypressEventSink(componentMock, dispatcher);
    dispatcher.addEventListener(EditKeypressEvent.class, e -> dispatchedEvent = e);
    sink.handleEvent(eventMock);

    assertEquals(eventMock.getKeyCode(), dispatchedEvent.getKeyCode());
    assertEquals(eventMock.getKeyCodeWithFlags(), dispatchedEvent.getKeyCodeWithFlags());
    assertEquals(eventMock.getModifiersEx(), dispatchedEvent.getExtendedModifiers());
    assertEquals(eventMock.isAltDown(), dispatchedEvent.isAltKey());
    assertEquals(eventMock.isCmdDown(), dispatchedEvent.isCmdKey());
    assertEquals(eventMock.isControlDown(), dispatchedEvent.isControlKey());
    assertEquals(eventMock.isShiftDown(), dispatchedEvent.isShiftKey());
  }
}


