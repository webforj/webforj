package org.dwcj.component.tabbedpane.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTabSelectedEvent;
import org.dwcj.component.tabbedpane.Tab;
import org.dwcj.component.tabbedpane.TabbedPane;
import org.dwcj.component.tabbedpane.sink.TabSelectEventSink;
import org.dwcj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TabSelectEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjTabSelectedEvent eventMock = Mockito.mock(BBjTabSelectedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TabSelectEvent[] dispatchedEvent = new TabSelectEvent[1];

    // Specify the return values for getIndex method
    when(eventMock.getIndex()).thenReturn(2);

    // Add event listener
    dispatcher.addListener(TabSelectEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    TabbedPane componentMock = Mockito.spy(TabbedPane.class);
    componentMock.addTab("Tab0");
    componentMock.addTab("Tab1");
    Tab tab2 = componentMock.addTab("Tab2");

    TabSelectEventSink sink = new TabSelectEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert indexes
    assertEquals(2, dispatchedEvent[0].getTabIndex());
    assertEquals(tab2, dispatchedEvent[0].getTab());
  }
}
