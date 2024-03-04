package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.IDwcjBBjBridge;
import com.webforj.exceptions.DwcjRuntimeException;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class PageTest {
  Page page;
  Environment environment;
  BBjAPI api;
  BBjWebManager webManager;
  BBjSysGui sysGui;
  IDwcjBBjBridge bridge;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    webManager = mock(BBjWebManager.class);
    sysGui = mock(BBjSysGui.class);
    bridge = mock(IDwcjBBjBridge.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getDwcjHelper()).thenReturn(bridge);
    when(api.getWebManager()).thenReturn(webManager);

    page = spy(Page.class);
    when(page.getEnvironment()).thenReturn(environment);
  }

  @Test
  void testExecuteJs() throws BBjException {
    String js = "console.log('script')";
    page.executeJs(js);
    verify(webManager).executeScript(js);
  }

  @Test
  void testExecuteJsAsync() {
    String script = "console.log('script')";
    PendingResult<Object> operation = page.executeJsAsync(script);

    assertNotNull(operation);
    assertFalse(operation.isDone());

    AtomicReference<String> resultRef = new AtomicReference<>();
    operation.complete("Executed: " + script);
    operation.thenAccept(result -> resultRef.set((String) result));

    assertEquals("Executed: " + script, resultRef.get());
    assertTrue(operation.isDone());
  }

  @Test
  void testReload() throws BBjException {
    page.reload();
    verify(page, times(1)).executeJsAsync("window.location.reload();");
  }


  @Nested
  class Title {

    @Test
    void shouldSetTitleWidthDefaultFormat() throws BBjException {
      page.setTitle("title");
      verify(webManager).setTitle(eq("title"), eq("{BrowserTitle}"), any(Map.class));
    }

    @Test
    void shouldSetTitleWidthCustomFormat() throws BBjException {
      String format = "{BrowserTitle} - {WindowTitle}";

      page.setTitle("title", format);
      verify(webManager).setTitle(eq("title"), eq(format), any(Map.class));
    }

    @Test
    void shouldSetTitleWidthCustomFormatAndPlaceholders() throws BBjException {
      String format = "{BrowserTitle} - {MyPlaceholder}";
      Map<String, String> placeholders = Map.of("MyPlaceholder", "MyPlaceholder Value");

      page.setTitle("title", format, placeholders);
      verify(webManager).setTitle(eq("title"), eq(format), eq(placeholders));
    }

    @Test
    void setTitleShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(webManager).setTitle(any(), any(), any());
      assertThrows(DwcjRuntimeException.class, () -> page.setTitle("title"));
    }

    @Test
    void shouldGetTitle() throws BBjException {
      when(webManager.getTitle()).thenReturn("title");
      assertEquals("title", page.getTitle());
    }

    @Test
    void getTitleShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(webManager).getTitle();
      assertThrows(DwcjRuntimeException.class, () -> page.getTitle());
    }
  }
}
