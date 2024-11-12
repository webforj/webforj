package com.webforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjClientFile;
import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.BBjThinClient;
import com.basis.bbj.proxies.BBjWebManager;
import com.basis.bbj.proxies.event.BBjWebEventOptions;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.event.page.PageUnloadEvent;
import com.webforj.exceptions.WebforjWebManagerException;
import java.io.File;
import java.io.InputStream;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class PageTest {
  Page page;
  Environment environment;
  BBjAPI api;
  BBjWebManager webManager;
  BBjSysGui sysGui;
  WebforjBBjBridge bridge;
  BBjThinClient thinClient;
  BBjClientFileSystem clientFileSystem;
  BBjClientFile clientFile;

  @BeforeEach
  void setUp() throws BBjException {
    environment = mock(Environment.class);
    api = mock(BBjAPI.class);
    webManager = mock(BBjWebManager.class);
    sysGui = mock(BBjSysGui.class);
    bridge = mock(WebforjBBjBridge.class);
    thinClient = mock(BBjThinClient.class);
    clientFileSystem = mock(BBjClientFileSystem.class);
    clientFile = mock(BBjClientFile.class);

    when(environment.getBBjAPI()).thenReturn(api);
    when(environment.getSysGui()).thenReturn(sysGui);
    when(environment.getWebforjHelper()).thenReturn(bridge);
    when(api.getWebManager()).thenReturn(webManager);
    when(api.getThinClient()).thenReturn(thinClient);
    when(thinClient.getClientFileSystem()).thenReturn(clientFileSystem);
    when(clientFileSystem.getClientFile(anyString())).thenReturn(clientFile);

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
  void shouldExecuteJsAsync() throws BBjException {
    String script = "console.log('script')";
    PendingResult<Object> operation = page.executeJsAsync(script);

    assertNotNull(operation);
    assertFalse(operation.isDone());

    AtomicReference<String> resultRef = new AtomicReference<>();
    operation.complete("Executed: " + script);
    operation.thenAccept(result -> resultRef.set((String) result));

    assertEquals("Executed: " + script, resultRef.get());
    assertTrue(operation.isDone());

    verify(webManager).executeAsyncScript(script, true, true);
  }

  @Test
  void shouldExecuteJsVoidAsync() throws BBjException {
    String script = "console.log('script')";
    page.executeJsVoidAsync(script);
    verify(webManager).executeAsyncScript(script, true, false);
  }

  @Test
  void testReload() {
    page.reload();
    verify(page, times(1)).executeJsAsync("window.location.reload();");
  }


  @Nested
  class Title {

    @Test
    void shouldSetTitleWidthDefaultFormat() throws BBjException {
      page.setTitle("title");
      verify(webManager).setTitle(eq("title"), eq(Page.DEFAULT_TITLE_FORMAT), any(Map.class));
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
      assertThrows(WebforjWebManagerException.class, () -> page.setTitle("title"));
    }

    @Test
    void shouldGetTitle() throws BBjException {
      when(webManager.getTitle()).thenReturn("title");
      assertEquals("title", page.getTitle());
    }

    @Test
    void getTitleShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(webManager).getTitle();
      assertThrows(WebforjWebManagerException.class, () -> page.getTitle());
    }
  }

  @Nested
  class FileDownload {

    @Test
    void shouldDownloadInputStream() throws BBjException {
      InputStream inputStream = mock(InputStream.class);
      page.download(inputStream, "file.txt");

      verify(clientFile).copyToClient(anyString());
    }

    @Test
    void downloadInputStreamShouldThrowException() throws BBjException {
      InputStream inputStream = mock(InputStream.class);
      doThrow(BBjException.class).when(clientFile).copyToClient(anyString());
      assertThrows(WebforjWebManagerException.class, () -> page.download(inputStream, "file.txt"));
    }

    @Test
    void shouldDownloadBytesArray() throws BBjException {
      byte[] bytes = new byte[0];
      page.download(bytes, "file.txt");

      verify(clientFile).copyToClient(anyString());
    }

    @Test
    void downloadBytesArrayShouldThrowException() throws BBjException {
      byte[] bytes = new byte[0];
      doThrow(BBjException.class).when(clientFile).copyToClient(anyString());
      assertThrows(WebforjWebManagerException.class, () -> page.download(bytes, "file.txt"));
    }

    @Test
    void shouldDownloadFile() throws BBjException {
      File file = mock(File.class);
      when(file.getAbsolutePath()).thenReturn("path/file.txt");
      when(file.getName()).thenReturn("file.txt");
      page.download(file);

      verify(clientFile).copyToClient("path/file.txt");
    }

    @Test
    void downloadFileShouldThrowException() throws BBjException {
      File file = mock(File.class);
      when(file.getAbsolutePath()).thenReturn("path/file.txt");
      when(file.getName()).thenReturn("file.txt");
      doThrow(BBjException.class).when(clientFile).copyToClient(anyString());

      assertThrows(WebforjWebManagerException.class, () -> page.download(file));
    }

    @Test
    void shouldDownloadFileWithAbsolutePath() throws BBjException {
      page.download("/path/file.txt", "file.txt");
      verify(clientFile).copyToClient("/path/file.txt");
    }
  }

  @Nested
  class OpenUrl {

    @Test
    void shouldOpenUrlWithNameAndFeatures() throws BBjException {
      page.open("http://example.com", "name", "features");
      verify(thinClient).browse("http://example.com", "name", "features");
    }

    @Test
    void shouldOpenUrlWithName() throws BBjException {
      page.open("http://example.com", "name");
      verify(thinClient).browse("http://example.com", "name", "");
    }

    @Test
    void shouldOpenUrl() throws BBjException {
      page.open("http://example.com");
      verify(thinClient).browse("http://example.com", "_blank", "");
    }
  }

  @Nested
  class UnloadListener {

    @Test
    void shouldAddUnloadListener() throws BBjException {
      EventListener<PageUnloadEvent> listener = mock(EventListener.class);
      ListenerRegistration<PageUnloadEvent> registration = page.onUnload(listener);
      assertNotNull(registration);

      verify(webManager, times(1)).setCallback(eq(SysGuiEventConstants.ON_BROWSER_CLOSE), any(),
          eq("onEvent"));
    }

    @Test
    void addUnloadListenerShouldThrowException() throws BBjException {
      doThrow(BBjException.class).when(webManager)
          .setCallback(eq(SysGuiEventConstants.ON_BROWSER_CLOSE), any(), eq("onEvent"));
      EventListener<PageUnloadEvent> listener = mock(EventListener.class);

      assertThrows(WebforjWebManagerException.class, () -> page.onUnload(listener));
    }
  }

  @Test
  void shouldAddEventWithDifferentParams() throws BBjException {
    try (MockedStatic<Environment> mockedEnvironment = mockStatic(Environment.class)) {
      mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);

      BBjWebEventOptions optionsMock = mock(BBjWebEventOptions.class);
      when(webManager.newEventOptions()).thenReturn(optionsMock);

      String type = "click";
      EventListener<PageEvent> listener = event -> {
      };
      PageEventOptions options1 = new PageEventOptions(null, "code", "filter");
      PageEventOptions options2 = new PageEventOptions(null, "code", null);

      ListenerRegistration<PageEvent> r1 = page.addEventListener(type, listener, options1);
      ListenerRegistration<PageEvent> r2 = page.addEventListener(type, listener, options2);
      ListenerRegistration<PageEvent> r3 = page.addEventListener(type, listener, options1);

      assertNotSame(r1, r2);
      assertNotSame(r1, r3);
      assertNotSame(r2, r3);
    }
  }
}
