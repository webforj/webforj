package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.Page;
import com.webforj.Request;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.Test;

class LiveReloadScriptInjectorTest {

  @Test
  void shouldBuildPlainSocketUrlForHttp() {
    assertEquals("ws://localhost:35730/webforj-devtools-ws", LiveReloadScriptInjector
        .resolveWebsocketUrl("http", "localhost", 35730, "/webforj-devtools-ws"));
  }

  @Test
  void shouldBuildSecureSocketUrlForHttps() {
    assertEquals("wss://example.com:40000/ws",
        LiveReloadScriptInjector.resolveWebsocketUrl("https", "example.com", 40000, "/ws"));
  }

  @Test
  void shouldDropPortAlreadyCarriedByHost() {
    assertEquals("ws://localhost:35730/ws",
        LiveReloadScriptInjector.resolveWebsocketUrl("http", "localhost:8080", 35730, "/ws"));
  }

  @Test
  void shouldComposeConfigurationAheadOfReloadScript() {
    String script = LiveReloadScriptInjector.composeScript("ws://localhost:35730/ws", 5000,
        "console.log('reload');");

    assertTrue(script.contains("websocketUrl: 'ws://localhost:35730/ws'"));
    assertTrue(script.contains("heartbeatInterval: 5000"));
    assertTrue(script.endsWith("console.log('reload');"));
  }

  @Test
  void shouldLoadAndCacheReloadScript() {
    LiveReloadScriptInjector injector = new LiveReloadScriptInjector();

    String first = injector.getReloadScript();
    String second = injector.getReloadScript();

    assertNotNull(first);
    assertTrue(first.contains("webforjDevToolsConfig"));
    assertSame(first, second);
  }

  @Test
  void shouldInjectScriptWhenEnabled() {
    Page page = mock(Page.class);
    Request request = mock(Request.class);
    when(request.getProtocol()).thenReturn("http");
    when(request.getHost()).thenReturn("localhost:8080");

    new LiveReloadScriptInjector()
        .inject(new LiveReloadOptions().setEnabled(true).setWebsocketPort(40000), page, request);

    verify(page).addInlineJavaScript(contains("ws://localhost:40000/webforj-devtools-ws"),
        eq(true));
  }

  @Test
  void shouldSkipInjectionQuietlyWhenTheSessionEndsDuringStartup() {
    Page page = mock(Page.class);
    Request request = mock(Request.class);
    when(request.getProtocol()).thenThrow(new WebforjRuntimeException("Failed to get the URL."));

    new LiveReloadScriptInjector()
        .inject(new LiveReloadOptions().setEnabled(true).setWebsocketPort(40000), page, request);

    verify(page, never()).addInlineJavaScript(anyString(), eq(true));
  }

  @Test
  void shouldNotInjectWhenDisabled() {
    Page page = mock(Page.class);
    Request request = mock(Request.class);

    new LiveReloadScriptInjector().inject(new LiveReloadOptions().setEnabled(false), page, request);

    verify(page, never()).addInlineJavaScript(anyString(), eq(true));
  }
}
