package com.webforj.component.element.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import com.basis.bbj.proxies.event.BBjWebComponentDefinedEvent;
import com.basis.startup.type.BBjException;
import com.webforj.component.element.Element;
import com.webforj.component.element.event.ElementDefinedEvent;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class ElementDefinedEventSinkTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() throws BBjException {
    BBjWebComponentDefinedEvent eventMock = Mockito.mock(BBjWebComponentDefinedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    ElementDefinedEvent[] dispatchedEvent = new ElementDefinedEvent[1];

    // Add event listener
    dispatcher.addListener(ElementDefinedEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    ElementDefinedEventSink sink = new ElementDefinedEventSink(mock(Element.class), dispatcher);
    sink.handleEvent(eventMock);

    assertEquals(1, dispatchedEvent.length);
  }
}

