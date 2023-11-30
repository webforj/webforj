package org.dwcj.component.event;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjExecuteScriptEvent;
import com.basis.startup.type.BBjException;
import org.dwcj.component.DwcComponentMock;
import org.dwcj.component.event.sink.ExecuteAsyncScriptEventSink;
import org.dwcj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ExecuteAsyncScriptEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() throws BBjException {
    BBjExecuteScriptEvent eventMock = Mockito.mock(BBjExecuteScriptEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    ExecuteAsyncScriptEvent[] dispatchedEvent = new ExecuteAsyncScriptEvent[1];

    // Specify the return values for getSelectedIndex and getSelectedIndices
    when(eventMock.getIndex()).thenReturn(2);
    when(eventMock.getResult()).thenReturn("result");

    // Add event listener
    dispatcher.addListener(ExecuteAsyncScriptEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    ExecuteAsyncScriptEventSink sink =
        new ExecuteAsyncScriptEventSink(new DwcComponentMock(), dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(2, dispatchedEvent[0].getIndex());
    assertEquals("result", dispatchedEvent[0].getResult());
  }
}
