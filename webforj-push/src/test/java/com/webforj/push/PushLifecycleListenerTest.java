package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.interaso.webpush.VapidKeys;
import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.environment.ObjectTable;
import com.webforj.push.exception.WebforjPushException;
import com.webforj.utilities.Assets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class PushLifecycleListenerTest {

  private final Map<String, Object> table = new HashMap<>();
  private MockedStatic<ObjectTable> mockedObjectTable;
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Page> mockedPage;
  private MockedStatic<Assets> mockedAssets;
  private Environment environment;
  private Page page;
  private final PushLifecycleListener listener = new PushLifecycleListener();
  private final App app = mock(App.class);

  @BeforeEach
  void setUp() {
    mockedObjectTable = mockStatic(ObjectTable.class);
    mockedObjectTable.when(() -> ObjectTable.contains(anyString()))
        .thenAnswer(invocation -> table.get(invocation.getArgument(0)) != null);
    mockedObjectTable.when(() -> ObjectTable.get(anyString()))
        .thenAnswer(invocation -> table.get(invocation.getArgument(0)));
    mockedObjectTable.when(() -> ObjectTable.put(anyString(), any())).thenAnswer(invocation -> {
      table.put(invocation.getArgument(0), invocation.getArgument(1));
      return invocation.getArgument(1);
    });

    environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory.empty());
    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    mockedEnvironment.when(Environment::isPresent).thenReturn(true);
    mockedEnvironment.when(Environment::getContextPath).thenReturn("/");
    mockedEnvironment.when(Environment::isRunningWithBBjServices).thenReturn(false);

    page = mock(Page.class);
    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
    mockedPage.when(Page::isPresent).thenReturn(true);

    mockedAssets = mockStatic(Assets.class);
    mockedAssets.when(() -> Assets.resolveWebServerUrl(Push.WORKER_URL))
        .thenReturn("/static/webforj/push/push-worker.min.js");
    mockedAssets.when(Assets::getIconsEndpoint).thenReturn("/icons/");
  }

  @AfterEach
  void tearDown() {
    Stream.of(mockedAssets, mockedPage, mockedEnvironment, mockedObjectTable)
        .filter(Objects::nonNull).forEach(MockedStatic::close);
  }


  @Test
  void shouldRegisterTheWorkerWhenConfigured() {
    configure();

    listener.onDidRun(app);

    ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
    verify(page, org.mockito.Mockito.atLeastOnce()).executeJsVoidAsync(captor.capture());
    List<String> scripts = captor.getAllValues();
    assertTrue(scripts.get(scripts.size() - 1).contains("\"command\":\"register\""),
        scripts.toString());
  }

  @Test
  void shouldDoNothingWhenNotConfigured() {
    listener.onDidRun(app);

    verify(page, never()).executeJsVoidAsync(anyString());
  }

  @Test
  void shouldFailAtStartupWhenPartiallyConfigured() {
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseMap(Map.of(PushConfiguration.PUBLIC_KEY, "pub")));

    assertThrows(WebforjPushException.class, () -> listener.onDidRun(app));
  }

  @Test
  void shouldFailLoudUnderTheHostedRuntimeWhenConfigured() {
    configure();
    mockedEnvironment.when(Environment::isRunningWithBBjServices).thenReturn(true);

    WebforjPushException e = assertThrows(WebforjPushException.class, () -> listener.onDidRun(app));

    assertEquals(PushStatus.UNSUPPORTED, e.getStatus());
    assertTrue(e.getMessage().contains("not supported"), e.getMessage());
    verify(page, never()).executeJsVoidAsync(anyString());
  }

  @Test
  void shouldStayQuietUnderTheHostedRuntimeWhenNotConfigured() {
    mockedEnvironment.when(Environment::isRunningWithBBjServices).thenReturn(true);

    listener.onDidRun(app);

    verify(page, never()).executeJsVoidAsync(anyString());
  }

  @Test
  void shouldSkipEmbeddedPages() {
    configure();
    when(page.isEmbedded()).thenReturn(true);

    listener.onDidRun(app);

    verify(page, never()).executeJsVoidAsync(anyString());
  }

  @Test
  void shouldSkipWhenThereIsNoPage() {
    configure();
    mockedPage.when(Page::isPresent).thenReturn(false);

    listener.onDidRun(app);

    verify(page, never()).executeJsVoidAsync(anyString());
  }

  @Test
  void shouldDestroyTheInstanceOnTerminate() {
    Push instance = Push.getCurrent();

    listener.onWillTerminate(app);

    assertNotSame(instance, Push.getCurrent(), "the instance was released");
  }

  @Test
  void shouldTerminateQuietlyWithoutAnEnvironment() {
    mockedEnvironment.when(Environment::isPresent).thenReturn(false);

    assertDoesNotThrow(() -> listener.onWillTerminate(app));
  }

  private void configure() {
    VapidKeys generated = VapidKeys.generate();
    when(environment.getConfig())
        .thenReturn(ConfigFactory.parseMap(Map.of(PushConfiguration.PUBLIC_KEY,
            generated.getX509PublicKey(), PushConfiguration.PRIVATE_KEY,
            generated.getPkcs8PrivateKey(), PushConfiguration.SUBJECT, "mailto:ops@example.com")));
  }
}
