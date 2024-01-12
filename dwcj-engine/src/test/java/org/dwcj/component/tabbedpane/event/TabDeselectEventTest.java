package org.dwcj.component.tabbedpane.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTabDeselectedEvent;
import org.dwcj.component.tabbedpane.Tab;
import org.dwcj.component.tabbedpane.TabbedPane;
import org.dwcj.component.tabbedpane.sink.TabDeselectEventSink;
import org.dwcj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TabDeselectEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjTabDeselectedEvent eventMock = Mockito.mock(BBjTabDeselectedEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TabDeselectEvent[] dispatchedEvent = new TabDeselectEvent[1];

    // Specify the return values for getIndex method
    when(eventMock.getIndex()).thenReturn(2);

    // Add event listener
    dispatcher.addListener(TabDeselectEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    TabbedPane componentMock = Mockito.spy(TabbedPane.class);
    componentMock.addTab("Tab0");
    componentMock.addTab("Tab1");
    Tab tab2 = componentMock.addTab("Tab2");

    TabDeselectEventSink sink = new TabDeselectEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert indexes
    assertEquals(2, dispatchedEvent[0].getTabIndex());
    assertEquals(tab2, dispatchedEvent[0].getTab());
  }
}
