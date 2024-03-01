package org.dwcj.component.navigator.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjNavigatorMovePreviousEvent;
import java.util.Map;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.sink.NavigatorMovePreviousEventSink;
import org.dwcj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class NavigatorMovePreviousEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjNavigatorMovePreviousEvent eventMock = Mockito.mock(BBjNavigatorMovePreviousEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    NavigatorMovePreviousEvent[] dispatchedEvent = new NavigatorMovePreviousEvent[1];

    // Specify the return values for getX() and getY()
    int current = 10;
    int startIndex = 20;
    int endIndex = 30;
    Map<String, Object> payload =
        Map.of("current", current, "startIndex", startIndex, "endIndex", endIndex);
    when(eventMock.getEventMap()).thenReturn(payload);

    when(eventMock.getEventMap()).thenReturn(payload);

    // Add event listener
    dispatcher.addListener(NavigatorMovePreviousEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Navigator componentMock = Mockito.mock(Navigator.class);
    NavigatorMovePreviousEventSink sink =
        new NavigatorMovePreviousEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert the payload values
    assertEquals(current, dispatchedEvent[0].getCurrent());
    assertEquals(startIndex, dispatchedEvent[0].getStartIndex());
    assertEquals(endIndex, dispatchedEvent[0].getEndIndex());
  }
}
