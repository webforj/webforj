package com.webforj.component.tabbedpane.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.event.BBjTabCloseEvent;
import com.webforj.component.tabbedpane.Tab;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.tabbedpane.sink.TabCloseEventSink;
import com.webforj.dispatcher.EventDispatcher;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class TabCloseEventTest {

  @Test
  @DisplayName("Test the payload of the event")
  void payload() {
    BBjTabCloseEvent eventMock = Mockito.mock(BBjTabCloseEvent.class);
    EventDispatcher dispatcher = new EventDispatcher();

    // Capture the dispatched event
    TabCloseEvent[] dispatchedEvent = new TabCloseEvent[1];

    // Specify the return values for getIndex method
    when(eventMock.getIndex()).thenReturn(2);

    // Add event listener
    dispatcher.addListener(TabCloseEvent.class, e -> {
      dispatchedEvent[0] = e;
    });

    TabbedPane componentMock = Mockito.spy(TabbedPane.class);
    componentMock.addTab("Tab0");
    componentMock.addTab("Tab1");
    Tab tab2 = componentMock.addTab("Tab2");

    TabCloseEventSink sink = new TabCloseEventSink(componentMock, dispatcher);
    // Invoke the event handler
    sink.handleEvent(eventMock);

    // Assert indexes
    assertEquals(2, dispatchedEvent[0].getTabIndex());
    assertEquals(tab2, dispatchedEvent[0].getTab());
  }
}
