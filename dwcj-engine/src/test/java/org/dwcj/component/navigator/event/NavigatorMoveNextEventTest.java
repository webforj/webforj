package org.dwcj.component.navigator.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjNavigatorMoveNextEvent;
import java.util.Map;
import org.dwcj.component.navigator.Navigator;
import org.dwcj.component.navigator.sink.NavigatorMoveNextEventSink;
import org.dwcj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class NavigatorMoveNextEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjNavigatorMoveNextEvent eventMock = Mockito.mock(BBjNavigatorMoveNextEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    NavigatorMoveNextEvent[] dispatchedEvent = new NavigatorMoveNextEvent[1];

    // Specify the return values for getX() and getY()
    int current = 10;
    int startIndex = 20;
    int endIndex = 30;
    Map<String, Object> payload =
        Map.of("current", current, "startIndex", startIndex, "endIndex", endIndex);
    when(eventMock.getEventMap()).thenReturn(payload);

    when(eventMock.getEventMap()).thenReturn(payload);

    // Add event listener
    dispatcher.addListener(NavigatorMoveNextEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Navigator componentMock = Mockito.mock(Navigator.class);
    NavigatorMoveNextEventSink sink = new NavigatorMoveNextEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert the payload values
    assertEquals(current, dispatchedEvent[0].getCurrent());
    assertEquals(startIndex, dispatchedEvent[0].getStartIndex());
    assertEquals(endIndex, dispatchedEvent[0].getEndIndex());
  }
}
