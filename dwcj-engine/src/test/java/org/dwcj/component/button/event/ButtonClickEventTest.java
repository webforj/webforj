package org.dwcj.component.button.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import org.dwcj.component.button.Button;
import org.dwcj.component.button.sink.ButtonClickEventSink;
import org.dwcj.component.event.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ButtonClickEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjButtonPushEvent eventMock = Mockito.mock(BBjButtonPushEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    ButtonClickEvent[] dispatchedEvent = new ButtonClickEvent[1];

    // Specify the return values for getX() and getY()
    when(eventMock.getX()).thenReturn(1d);
    when(eventMock.getY()).thenReturn(2d);

    // Add event listener
    dispatcher.addEventListener(ButtonClickEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Button componentMock = Mockito.mock(Button.class);
    ButtonClickEventSink sink = new ButtonClickEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert the payload values
    assertEquals(1d, dispatchedEvent[0].getX());
    assertEquals(2d, dispatchedEvent[0].getY());
  }
}
