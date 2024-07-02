package com.webforj.component.slider.event;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjControlScrollEvent;
import com.webforj.component.slider.Slider;
import com.webforj.component.slider.sink.SliderSlideEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class SliderSlideEventTest {

  @Test
  void payload() {
    BBjControlScrollEvent eventMock = Mockito.mock(BBjControlScrollEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    SliderSlideEvent[] dispatchedEvent = new SliderSlideEvent[1];

    // Specify the return values for getSelectedIndex and getSelectedIndices
    when(eventMock.getPosition()).thenReturn(2);
    when(eventMock.isAdjusting()).thenReturn(true);

    // Add event listener
    dispatcher.addListener(SliderSlideEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    Slider componentMock = Mockito.spy(Slider.class);
    SliderSlideEventSink sink = new SliderSlideEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    assertEquals(2, dispatchedEvent[0].getValue());
    assertTrue(dispatchedEvent[0].isAdjusting());
  }
}
