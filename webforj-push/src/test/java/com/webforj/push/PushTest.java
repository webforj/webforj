package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.interaso.webpush.VapidKeys;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.typesafe.config.ConfigValueFactory;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.PendingResult;
import com.webforj.environment.ObjectTable;
import com.webforj.push.exception.WebforjPushException;
import com.webforj.router.Router;
import com.webforj.utilities.Assets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletionException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;

class PushTest {

  private static final String WORKER = "/crm/static/webforj/push/push-worker.min.js";

  private final Map<String, Object> table = new HashMap<>();
  private final List<PendingResult<Object>> answers = new ArrayList<>();
  private MockedStatic<ObjectTable> mockedObjectTable;
  private MockedStatic<Environment> mockedEnvironment;
  private MockedStatic<Page> mockedPage;
  private MockedStatic<Assets> mockedAssets;
  private Environment environment;
  private Page page;
  private Config config;

  private static Config configured() {
    VapidKeys generated = VapidKeys.generate();
    return ConfigFactory.parseMap(Map.of(PushConfiguration.PUBLIC_KEY, generated.getX509PublicKey(),
        PushConfiguration.PRIVATE_KEY, generated.getPkcs8PrivateKey(), PushConfiguration.SUBJECT,
        "mailto:ops@example.com"));
  }

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

    config = configured();
    environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(config);
    mockedEnvironment = mockStatic(Environment.class);
    mockedEnvironment.when(Environment::getCurrent).thenReturn(environment);
    mockedEnvironment.when(Environment::isPresent).thenReturn(true);
    mockedEnvironment.when(Environment::getContextPath).thenReturn("/crm");
    mockedEnvironment.when(Environment::isRunningWithBBjServices).thenReturn(false);

    page = mock(Page.class);
    when(page.executeJsAsync(anyString())).thenAnswer(invocation -> {
      PendingResult<Object> answer = new PendingResult<>();
      answers.add(answer);
      return answer;
    });
    mockedPage = mockStatic(Page.class);
    mockedPage.when(Page::getCurrent).thenReturn(page);
    mockedPage.when(Page::isPresent).thenReturn(true);

    mockedAssets = mockStatic(Assets.class);
    mockedAssets.when(() -> Assets.resolveWebServerUrl(Push.WORKER_URL)).thenReturn(WORKER);
    mockedAssets.when(Assets::getIconsEndpoint).thenReturn("/crm/icons/");
  }

  @AfterEach
  void tearDown() {
    Stream.of(mockedAssets, mockedPage, mockedEnvironment, mockedObjectTable)
        .filter(Objects::nonNull).forEach(MockedStatic::close);
  }

  private String lastCall() {
    ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
    verify(page, org.mockito.Mockito.atLeastOnce()).executeJsAsync(captor.capture());
    return captor.getValue();
  }

  private String lastVoidScript() {
    ArgumentCaptor<String> captor = ArgumentCaptor.forClass(String.class);
    verify(page, org.mockito.Mockito.atLeastOnce()).executeJsVoidAsync(captor.capture());
    return captor.getValue();
  }

  private void answer(String envelope) {
    answers.get(answers.size() - 1).complete(envelope);
  }

  private static <T> WebforjPushException failureOf(PendingResult<T> result) {
    AtomicReference<Throwable> failure = new AtomicReference<>();
    result.exceptionally(e -> {
      failure.set(e instanceof CompletionException ? e.getCause() : e);
      return null;
    });
    assertNotNull(failure.get(), "the result failed");
    return (WebforjPushException) failure.get();
  }

  private static <T> T valueOf(PendingResult<T> result) {
    AtomicReference<T> value = new AtomicReference<>();
    result.thenAccept(value::set);
    return value.get();
  }

  @Nested
  class Access {

    @Test
    void shouldKeepOneInstancePerEnvironment() {
      Push first = Push.getCurrent();

      assertSame(first, Push.getCurrent());
      assertSame(first, table.get(Push.class.getName()));
    }

    @Test
    void shouldBePresentWhenAnInstanceExists() {
      assertTrue(Push.isPresent());
    }

    @Test
    void shouldRunTheConsumerWithTheInstance() {
      AtomicReference<Push> seen = new AtomicReference<>();
      Push.ifPresent(seen::set);

      assertSame(Push.getCurrent(), seen.get());
    }

    @Test
    void shouldReleaseTheInstanceOnDestroy() {
      Push instance = Push.getCurrent();

      instance.destroy();

      assertNotSame(instance, Push.getCurrent(), "a fresh instance follows destroy");
      assertTrue(instance.isDestroyed());
      assertFalse(Push.getCurrent().isDestroyed());
    }
  }

  @Nested
  class Bridge {

    @Test
    void shouldInstallTheStubAndLinkTheScriptOnce() {
      Push push = Push.getCurrent();
      push.getPermission();
      push.getPermission();

      verify(page, times(1)).addJavaScript(Push.SCRIPT_URL, true);
      verify(page, times(1)).executeJsVoidAsync(anyString());
      assertTrue(lastVoidScript().contains("window.__webforjPush = window.__webforjPush ||"),
          lastVoidScript());
      assertTrue(lastVoidScript().contains("new Promise"), lastVoidScript());
      assertEquals(Boolean.TRUE, table.get(Push.ASSET_KEY));
    }

    @Test
    void shouldCallTheBridgeThroughTheAwaitingScriptExecution() {
      Push.getCurrent().getPermission();

      assertTrue(lastCall().startsWith("window.__webforjPush.call({"), lastCall());
      assertTrue(lastCall().endsWith("})"), lastCall());
    }

    @Test
    void shouldRefuseTheHostedRuntime() {
      mockedEnvironment.when(Environment::isRunningWithBBjServices).thenReturn(true);

      Push push = Push.getCurrent();
      WebforjPushException e = assertThrows(WebforjPushException.class, push::getPermission);

      assertEquals(PushStatus.UNSUPPORTED, e.getStatus());
      verify(page, never()).executeJsAsync(anyString());
    }

    @Test
    void shouldFailWhenTheBrowserReturnsNoAnswer() {
      PendingResult<PushPermission> result = Push.getCurrent().getPermission();

      answer(null);

      assertEquals(PushStatus.UNKNOWN, failureOf(result).getStatus());
    }
  }

  @Nested
  class WorkerRequest {

    @Test
    void shouldDeriveWorkerScopeAndBasesFromTheDeployment() {
      Push.getCurrent().registerServiceWorker();

      String script = lastVoidScript();
      assertTrue(script.contains("\"command\":\"register\""), script);
      assertTrue(script.contains("\"scope\":\"/crm/static/webforj/push/\""), script);
      assertTrue(
          script.contains("\"worker\":\"" + WORKER + "?root=%2Fcrm&icons=%2Fcrm%2Ficons%2F&v="),
          script);
    }

    @Test
    void shouldPreferTheRouterRootOverTheContextPath() {
      Router router = mock(Router.class);
      when(router.getRoot()).thenReturn(Optional.of("/app"));
      table.put(Router.class.getName(), router);

      Push.getCurrent().registerServiceWorker();

      assertTrue(lastVoidScript().contains("?root=%2Fapp&"), lastVoidScript());
    }

    @Test
    void shouldFallBackToTheConfiguredRouterRoot() {
      when(environment.getConfig())
          .thenReturn(config.withValue("webforj.router.root", ConfigValueFactory.fromAnyRef("/")));

      Push.getCurrent().registerServiceWorker();

      assertTrue(lastVoidScript().contains("?root=%2F&"), lastVoidScript());
    }
  }

  @Nested
  class Subscribe {

    @Test
    void shouldSendTheApplicationServerKeyAndResolveTheSubscription() {
      final PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

      String script = lastCall();
      assertTrue(script.contains("\"command\":\"subscribe\""), script);
      assertTrue(script.contains("\"key\":\""), script);
      assertFalse(script.contains(config.getString(PushConfiguration.PRIVATE_KEY)), script);

      answer("{\"ok\":true,\"value\":{\"endpoint\":\"https://push.example/1\",\"p256dh\":\"k\","
          + "\"auth\":\"a\"}}");

      PushSubscription subscription = valueOf(result);
      assertEquals("https://push.example/1", subscription.getEndpoint());
      assertEquals("k", subscription.getP256dh());
      assertEquals("a", subscription.getAuth());
    }

    @Test
    void shouldFailWhenTheUserDeniedNotifications() {
      PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

      answer("{\"ok\":false,\"error\":\"permission-denied\",\"message\":\"no\"}");

      WebforjPushException failure = failureOf(result);
      assertEquals(PushStatus.PERMISSION_DENIED, failure.getStatus());
      assertEquals("no", failure.getMessage());
    }

    @Test
    void shouldFailWhenTheBrowserCannotReceivePushes() {
      PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

      answer("{\"ok\":false,\"error\":\"unsupported\",\"message\":\"old\"}");

      assertEquals(PushStatus.UNSUPPORTED, failureOf(result).getStatus());
    }

    @Test
    void shouldFailWithTheBrowserMessageOnOtherErrors() {
      PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

      answer("{\"ok\":false,\"error\":\"failed\",\"message\":\"boom\"}");

      WebforjPushException failure = failureOf(result);
      assertEquals(PushStatus.UNKNOWN, failure.getStatus());
      assertEquals("boom", failure.getMessage());
    }

    @Test
    void shouldFailWithUnknownWhenTheBrowserAnswerIsNotAnObject() {
      for (String answer : new String[] {"null", "undefined", "true", "{not json"}) {
        PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

        answer(answer);

        assertEquals(PushStatus.UNKNOWN, failureOf(result).getStatus(), answer);
      }
    }

    @Test
    void shouldFailWhenTheBrowserReturnsNoSubscription() {
      PendingResult<PushSubscription> result = Push.getCurrent().subscribe();

      answer("{\"ok\":true,\"value\":null}");

      assertEquals(PushStatus.UNKNOWN, failureOf(result).getStatus());
    }

    @Test
    void shouldFailBeforeCallingTheBrowserWhenNotConfigured() {
      when(environment.getConfig()).thenReturn(ConfigFactory.empty());

      Push push = Push.getCurrent();
      WebforjPushException e = assertThrows(WebforjPushException.class, push::subscribe);

      assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
      assertTrue(e.getMessage().contains(PushConfiguration.PUBLIC_KEY), e.getMessage());
      verify(page, never()).executeJsAsync(anyString());
    }
  }

  @Nested
  class UnsubscribeAndLookup {

    @Test
    void shouldReturnTheCancelledSubscription() {
      PendingResult<Optional<PushSubscription>> result = Push.getCurrent().unsubscribe();

      assertTrue(lastCall().contains("\"command\":\"unsubscribe\""), lastCall());
      answer("{\"ok\":true,\"value\":{\"endpoint\":\"https://push.example/1\",\"p256dh\":\"k\","
          + "\"auth\":\"a\"}}");

      assertEquals("https://push.example/1", valueOf(result).orElseThrow().getEndpoint());
    }

    @Test
    void shouldBeEmptyWhenTheBrowserHadNoSubscription() {
      PendingResult<Optional<PushSubscription>> result = Push.getCurrent().unsubscribe();

      answer("{\"ok\":true,\"value\":null}");

      assertTrue(valueOf(result).isEmpty());
    }

    @Test
    void shouldLookUpTheExistingSubscription() {
      PendingResult<Optional<PushSubscription>> result = Push.getCurrent().getSubscription();

      assertTrue(lastCall().contains("\"command\":\"getSubscription\""), lastCall());
      answer("{\"ok\":true,\"value\":{\"endpoint\":\"https://push.example/2\",\"p256dh\":\"k\","
          + "\"auth\":\"a\"}}");

      assertEquals("https://push.example/2", valueOf(result).orElseThrow().getEndpoint());
    }

    @Test
    void shouldBeEmptyWhenTheBrowserHoldsNoSubscription() {
      PendingResult<Optional<PushSubscription>> result = Push.getCurrent().getSubscription();

      answer("{\"ok\":true}");

      assertTrue(valueOf(result).isEmpty());
    }
  }

  @Nested
  class Permission {

    @Test
    void shouldMapTheBrowserPermission() {
      PendingResult<PushPermission> result = Push.getCurrent().getPermission();

      assertTrue(lastCall().contains("\"command\":\"getPermission\""), lastCall());
      answer("{\"ok\":true,\"value\":\"denied\"}");

      assertEquals(PushPermission.DENIED, valueOf(result));
    }

    @Test
    void shouldFallBackToPromptWithoutTheValue() {
      PendingResult<PushPermission> result = Push.getCurrent().getPermission();

      answer("{\"ok\":true,\"value\":null}");

      assertEquals(PushPermission.PROMPT, valueOf(result));
    }
  }
}
