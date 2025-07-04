package com.webforj.spring.devtools.livereload;

import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.App;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.spring.ContextHolder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.Environment;

class DevToolsScriptInjectorTest {

  private LiveReloadScriptInjector injector;
  private App mockApp;
  private Page mockPage;
  private Request mockRequest;
  private ApplicationContext mockContext;
  private Environment mockEnvironment;

  @BeforeEach
  void setUp() {
    injector = new LiveReloadScriptInjector();
    mockApp = mock(App.class);
    mockPage = mock(Page.class);
    mockRequest = mock(Request.class);
    mockContext = mock(ApplicationContext.class);
    mockEnvironment = mock(Environment.class);

    when(mockApp.getId()).thenReturn("test-app");
    when(mockContext.getEnvironment()).thenReturn(mockEnvironment);
  }

  @Test
  void shouldInjectScriptWhenDevToolsEnabledAndPresent() {
    try (MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {

      contextMock.when(ContextHolder::getContext).thenReturn(mockContext);
      when(mockEnvironment.getProperty("webforj.devtools.livereload.enabled")).thenReturn("true");

      pageMock.when(Page::getCurrent).thenReturn(mockPage);
      requestMock.when(Request::getCurrent).thenReturn(mockRequest);

      when(mockRequest.getProtocol()).thenReturn("http");
      when(mockRequest.getHost()).thenReturn("localhost:8080");

      injector.onDidRun(mockApp);
      verify(mockPage).addInlineJavaScript(contains("webforjDevToolsConfig"), eq(true));
    }
  }

  @Test
  void shouldNotInjectScriptWhenDevToolsDisabled() {
    try (MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {
      contextMock.when(ContextHolder::getContext).thenReturn(mockContext);
      when(mockEnvironment.getProperty("webforj.devtools.livereload.enabled")).thenReturn("false");

      injector.onDidRun(mockApp);
      verify(mockPage, never()).addInlineJavaScript(anyString(), anyBoolean());
    }
  }

  @Test
  void shouldHandleSecureWebSocketProtocol() {
    try (MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {

      contextMock.when(ContextHolder::getContext).thenReturn(mockContext);
      when(mockEnvironment.getProperty("webforj.devtools.livereload.enabled")).thenReturn("true");

      pageMock.when(Page::getCurrent).thenReturn(mockPage);
      requestMock.when(Request::getCurrent).thenReturn(mockRequest);

      when(mockRequest.getProtocol()).thenReturn("https");
      when(mockRequest.getHost()).thenReturn("example.com:443");

      injector.onDidRun(mockApp);
      verify(mockPage).addInlineJavaScript(contains("wss://"), eq(true));
    }
  }

  @Test
  void shouldUseCustomWebSocketPortFromConfiguration() {
    try (MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {

      contextMock.when(ContextHolder::getContext).thenReturn(mockContext);
      when(mockEnvironment.getProperty("webforj.devtools.livereload.enabled")).thenReturn("true");
      when(mockEnvironment.getProperty("webforj.devtools.livereload.websocketPort"))
          .thenReturn("12345");

      pageMock.when(Page::getCurrent).thenReturn(mockPage);
      requestMock.when(Request::getCurrent).thenReturn(mockRequest);

      when(mockRequest.getProtocol()).thenReturn("http");
      when(mockRequest.getHost()).thenReturn("localhost");

      injector.onDidRun(mockApp);
      verify(mockPage).addInlineJavaScript(contains(":12345"), eq(true));
    }
  }

  @Test
  void shouldFallbackToSystemPropertyWhenSpringContextNotAvailable() {
    try (MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {

      contextMock.when(ContextHolder::getContext).thenReturn(null);
      System.setProperty("webforj.devtools.livereload.enabled", "true");

      pageMock.when(Page::getCurrent).thenReturn(mockPage);
      requestMock.when(Request::getCurrent).thenReturn(mockRequest);

      when(mockRequest.getProtocol()).thenReturn("http");
      when(mockRequest.getHost()).thenReturn("localhost");

      try {
        injector.onDidRun(mockApp);
        verify(mockPage).addInlineJavaScript(contains("webforjDevToolsConfig"), eq(true));
      } finally {
        System.clearProperty("webforj.devtools.livereload.enabled");
      }
    }
  }

  @Test
  void shouldExtractHostnameFromHostWithPort() {
    try (MockedStatic<Page> pageMock = mockStatic(Page.class);
        MockedStatic<Request> requestMock = mockStatic(Request.class);
        MockedStatic<ContextHolder> contextMock = mockStatic(ContextHolder.class)) {

      contextMock.when(ContextHolder::getContext).thenReturn(mockContext);
      when(mockEnvironment.getProperty("webforj.devtools.livereload.enabled")).thenReturn("true");

      pageMock.when(Page::getCurrent).thenReturn(mockPage);
      requestMock.when(Request::getCurrent).thenReturn(mockRequest);

      when(mockRequest.getProtocol()).thenReturn("http");
      when(mockRequest.getHost()).thenReturn("example.com:8080");

      injector.onDidRun(mockApp);
      verify(mockPage).addInlineJavaScript(contains("ws://example.com:35730"), eq(true));
    }
  }
}
