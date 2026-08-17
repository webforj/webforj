package com.webforj.mcp;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.Page;
import com.webforj.dispatcher.EventListener;
import com.webforj.environment.ObjectTable;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class McpHostListenerTest {

  private final McpHostListener listener = new McpHostListener();
  private final App app = mock(App.class);
  private final Page page = mock(Page.class);

  @Test
  @DisplayName("Should wire the page messages into a host on an embedded run")
  void shouldWireEmbeddedRun() {
    try (MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class)) {
      pages.when(Page::isPresent).thenReturn(true);
      pages.when(Page::getCurrent).thenReturn(page);
      when(page.isEmbedded()).thenReturn(true);

      listener.onWillRun(app);

      table.verify(() -> ObjectTable.put(eq(McpHost.OBJECT_TABLE_KEY), any(McpHost.class)));
      verify(page).addEventListener(eq("webforj-mcp-message"), any(EventListener.class),
          any(PageEventOptions.class));
    }
  }

  @Test
  @DisplayName("Should do nothing in a regular run")
  void shouldDoNothingWhenNotEmbedded() {
    try (MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class)) {
      pages.when(Page::isPresent).thenReturn(true);
      pages.when(Page::getCurrent).thenReturn(page);
      when(page.isEmbedded()).thenReturn(false);

      listener.onWillRun(app);

      table.verify(() -> ObjectTable.put(any(), any()), never());
      verify(page, never()).addEventListener(eq("webforj-mcp-message"), any(EventListener.class),
          any(PageEventOptions.class));
    }
  }

  @Test
  @DisplayName("Should flush the channel once the application ran")
  void shouldSignalReadyAfterRun() {
    McpHost host = mock(McpHost.class);
    try (MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class)) {
      table.when(() -> ObjectTable.contains(McpHost.OBJECT_TABLE_KEY)).thenReturn(true);
      table.when(() -> ObjectTable.get(McpHost.OBJECT_TABLE_KEY)).thenReturn(host);

      listener.onDidRun(app);

      verify(host).signalReady();
    }
  }

  @Test
  @DisplayName("Should destroy the host when the application terminates")
  void shouldDestroyHostOnTerminate() {
    McpHost host = mock(McpHost.class);
    try (MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class)) {
      table.when(() -> ObjectTable.contains(McpHost.OBJECT_TABLE_KEY)).thenReturn(true);
      table.when(() -> ObjectTable.get(McpHost.OBJECT_TABLE_KEY)).thenReturn(host);

      listener.onWillTerminate(app);

      verify(host).destroy();
    }
  }

  @Test
  @DisplayName("Should dispatch a page message payload into the host")
  void shouldDispatchPagePayload() {
    try (MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class)) {
      pages.when(Page::isPresent).thenReturn(true);
      pages.when(Page::getCurrent).thenReturn(page);
      when(page.isEmbedded()).thenReturn(true);

      listener.onWillRun(app);

      @SuppressWarnings("unchecked")
      ArgumentCaptor<EventListener<PageEvent>> pageListener =
          ArgumentCaptor.forClass(EventListener.class);
      verify(page).addEventListener(eq("webforj-mcp-message"), pageListener.capture(),
          any(PageEventOptions.class));

      PageEvent event = mock(PageEvent.class);
      when(event.getData()).thenReturn(
          Map.of("payload", "{\"type\":\"tool-cancelled\",\"payload\":{\"reason\":\"done\"}}"));
      assertDoesNotThrow(() -> pageListener.getValue().onEvent(event),
          "the payload must parse into the host");
    }
  }
}
