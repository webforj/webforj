package com.webforj.component.navigator.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjNavigatorMoveFirstEvent;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.navigator.sink.NavigatorMoveFirstEventSink;
import com.webforj.dispatcher.EventDispatcher;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class NavigatorMoveFirstEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjNavigatorMoveFirstEvent eventMock = Mockito.mock(BBjNavigatorMoveFirstEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    NavigatorMoveFirstEvent[] dispatchedEvent = new NavigatorMoveFirstEvent[1];

    // Specify the return values for getX() and getY()
    int current = 10;
    int startIndex = 20;
    int endIndex = 30;
    Map<String, Object> payload =
        Map.of("current", current, "startIndex", startIndex, "endIndex", endIndex);
    when(eventMock.getEventMap()).thenReturn(payload);

    when(eventMock.getEventMap()).thenReturn(payload);

    // Add event listener
    dispatcher.addListener(NavigatorMoveFirstEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Navigator componentMock = Mockito.mock(Navigator.class);
    NavigatorMoveFirstEventSink sink = new NavigatorMoveFirstEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert the payload values
    assertEquals(current, dispatchedEvent[0].getCurrent());
    assertEquals(startIndex, dispatchedEvent[0].getStartIndex());
    assertEquals(endIndex, dispatchedEvent[0].getEndIndex());
  }
}
