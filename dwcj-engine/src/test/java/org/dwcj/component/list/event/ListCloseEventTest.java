package org.dwcj.component.list.event;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjListCloseEvent;
import com.basis.startup.type.BBjVector;
import java.util.List;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.component.list.ComboBox;
import org.dwcj.component.list.sink.ListCloseEventSink;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ListCloseEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjListCloseEvent eventMock = Mockito.mock(BBjListCloseEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    ListCloseEvent[] dispatchedEvent = new ListCloseEvent[1];

    // Specify the return values for getSelectedIndex and getSelectedIndices
    when(eventMock.getSelectedIndex()).thenReturn(2);
    when(eventMock.getSelectedIndices()).thenReturn(new BBjVector(List.of(2)));

    // Add event listener
    dispatcher.addListener(ListCloseEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    ComboBox componentMock = Mockito.spy(ComboBox.class);
    componentMock.insert("Item 1", "Item 2", "Item 3");
    ListCloseEventSink sink = new ListCloseEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert indexes
    assertEquals(2, dispatchedEvent[0].getSelectedIndex());
    assertEquals(List.of(2), dispatchedEvent[0].getSelectedIndices());

    // Assert single item
    assertEquals(componentMock.getByIndex(2), dispatchedEvent[0].getSelectedItem());

    // Assert multiple items
    assertEquals(1, dispatchedEvent[0].getSelectedItems().size());
    assertTrue(dispatchedEvent[0].getSelectedItems().contains(componentMock.getByIndex(2)));
  }
}
