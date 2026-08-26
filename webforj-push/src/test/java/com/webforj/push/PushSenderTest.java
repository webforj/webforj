package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.interaso.webpush.VapidKeys;
import com.sun.net.httpserver.HttpServer;
import com.typesafe.config.ConfigFactory;
import com.webforj.Environment;
import com.webforj.PendingResult;
import com.webforj.environment.ObjectTable;
import com.webforj.push.exception.WebforjPushException;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.security.SecureRandom;
import java.time.Duration;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.MockedStatic;

class PushSenderTest {

  private HttpServer server;
  private final AtomicInteger status = new AtomicInteger(201);
  private final Map<String, String> receivedHeaders = new ConcurrentHashMap<>();
  private final AtomicInteger receivedBodyLength = new AtomicInteger();
  private final AtomicReference<CountDownLatch> gate = new AtomicReference<>(new CountDownLatch(0));
  private PushSender sender;
  private PushSubscription subscription;

  @BeforeEach
  void setUp() throws IOException {
    server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    server.createContext("/push", exchange -> {
      exchange.getRequestHeaders()
          .forEach((name, values) -> receivedHeaders.put(name, String.join(",", values)));
      receivedBodyLength.set(exchange.getRequestBody().readAllBytes().length);
      try {
        gate.get().await(5, TimeUnit.SECONDS);
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
      exchange.sendResponseHeaders(status.get(), -1);
      exchange.close();
    });
    server.start();

    sender = new PushSender(keys(), "mailto:ops@example.com");

    byte[] auth = new byte[16];
    new SecureRandom().nextBytes(auth);
    String p256dh = Base64.getUrlEncoder().withoutPadding()
        .encodeToString(VapidKeys.generate().getApplicationServerKey());
    subscription =
        new PushSubscription("http://127.0.0.1:" + server.getAddress().getPort() + "/push", p256dh,
            Base64.getUrlEncoder().withoutPadding().encodeToString(auth));
  }

  @AfterEach
  void tearDown() {
    gate.get().countDown();
    server.stop(0);
  }

  @Nested
  class Construction {

    @Test
    void shouldReadTheConfigurationOfTheCurrentEnvironment() {
      VapidKeys vapid = VapidKeys.generate();
      Environment environment = mock(Environment.class);
      when(environment.getConfig())
          .thenReturn(ConfigFactory.parseMap(Map.of(PushConfiguration.PUBLIC_KEY,
              vapid.getX509PublicKey(), PushConfiguration.PRIVATE_KEY, vapid.getPkcs8PrivateKey(),
              PushConfiguration.SUBJECT, "mailto:ops@example.com")));

      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(Environment::getCurrent).thenReturn(environment);

        PushSender created = new PushSender();

        assertEquals(vapid.getX509PublicKey(), created.getKeys().getPublicKey());
        assertEquals("mailto:ops@example.com", created.getSubject());
      }
    }

    @Test
    void shouldFailWithoutAnEnvironmentOnTheThread() {
      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(Environment::getCurrent).thenReturn(null);

        WebforjPushException e = assertThrows(WebforjPushException.class, PushSender::new);

        assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
        assertTrue(e.getMessage().contains("application thread"), e.getMessage());
      }
    }

    @Test
    void shouldFailWhenTheEnvironmentIsNotConfigured() {
      Environment environment = mock(Environment.class);
      when(environment.getConfig()).thenReturn(ConfigFactory.empty());

      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(Environment::getCurrent).thenReturn(environment);

        WebforjPushException e = assertThrows(WebforjPushException.class, PushSender::new);

        assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
        assertTrue(e.getMessage().contains("webforj:push-keys"), e.getMessage());
      }
    }

    @Test
    void shouldExposeKeysAndSubject() {
      PushKeys keys = keys();
      PushSender created = new PushSender(keys, "https://example.com");

      assertSame(keys, created.getKeys());
      assertEquals("https://example.com", created.getSubject());
    }

    @Test
    void shouldAcceptTheConfiguration() {
      PushConfiguration configuration = new PushConfiguration(keys(), "mailto:ops@example.com");

      assertEquals("mailto:ops@example.com", new PushSender(configuration).getSubject());
    }

    @Test
    void shouldDefaultTheTimeout() {
      assertEquals(PushSender.DEFAULT_TIMEOUT, sender.getTimeout());
    }

    @Test
    void shouldSetTheTimeout() {
      assertEquals(Duration.ofSeconds(3), sender.setTimeout(Duration.ofSeconds(3)).getTimeout());
    }

    @Test
    void shouldRejectTheMissingTimeout() {
      assertThrows(NullPointerException.class, () -> sender.setTimeout(null));
    }

    @Test
    void shouldRejectMalformedKeys() {
      PushKeys malformed = new PushKeys("x", "y");
      assertThrows(WebforjPushException.class,
          () -> new PushSender(malformed, "mailto:ops@example.com"));
    }

    @Test
    void shouldRejectTheBadSubject() {
      PushKeys keys = keys();
      assertThrows(WebforjPushException.class, () -> new PushSender(keys, "ops@example.com"));
    }

    @Test
    void shouldRejectMissingConfiguration() {
      assertThrows(NullPointerException.class, () -> new PushSender(null));
    }
  }

  @Nested
  class Send {

    @Test
    void shouldReturnBeforeThePushServiceAnswers() throws Exception {
      CountDownLatch answer = new CountDownLatch(1);
      gate.set(answer);
      status.set(201);

      PendingResult<Void> result = sender.send(subscription, PushMessage.create("Title").build());

      assertFalse(result.isDone(), "send must not wait for the push service");
      answer.countDown();
      assertNull(await(result));
    }

    @Test
    void shouldDeliverAnEncryptedSignedRequest() throws Exception {
      status.set(201);
      PushMessage message =
          PushMessage.create("Title").setBody("Body").setTimeToLive(Duration.ofMinutes(5))
              .setUrgency(PushUrgency.HIGH).setTopic("orders").build();

      assertNull(await(sender.send(subscription, message)));

      assertTrue(receivedHeaders.get("Authorization").startsWith("vapid t="),
          receivedHeaders.toString());
      assertEquals("aes128gcm", receivedHeaders.get("Content-encoding"));
      assertEquals("300", receivedHeaders.get("Ttl"));
      assertEquals("high", receivedHeaders.get("Urgency"));
      assertEquals("orders", receivedHeaders.get("Topic"));
      assertTrue(receivedBodyLength.get() > 100, "the body carries the encrypted payload");
    }

    @ParameterizedTest
    @CsvSource({"VERY_LOW, very-low", "LOW, low", "NORMAL, normal", "HIGH, high"})
    void shouldSendEveryUrgency(PushUrgency urgency, String header) throws Exception {
      status.set(201);
      PushMessage message = PushMessage.create("Title").setUrgency(urgency).build();

      assertNull(await(sender.send(subscription, message)));

      assertEquals(header, receivedHeaders.get("Urgency"));
    }

    @Test
    void shouldAcceptEveryAcceptedStatus() throws Exception {
      for (int accepted : new int[] {200, 201, 202}) {
        status.set(accepted);
        assertNull(await(sender.send(subscription, PushMessage.create("Title").build())));
      }
    }

    @Test
    void shouldReportAnExpiredSubscriptionOn410() throws Exception {
      status.set(410);

      WebforjPushException e =
          await(sender.send(subscription, PushMessage.create("Title").build()));

      assertEquals(PushStatus.SUBSCRIPTION_EXPIRED, e.getStatus());
      assertTrue(e.getMessage().contains(subscription.getEndpoint()), e.getMessage());
    }

    @Test
    void shouldReportAnExpiredSubscriptionOn404() throws Exception {
      status.set(404);

      WebforjPushException e =
          await(sender.send(subscription, PushMessage.create("Title").build()));

      assertEquals(PushStatus.SUBSCRIPTION_EXPIRED, e.getStatus());
    }

    @Test
    void shouldCarryTheStatusOfTheRefusal() throws Exception {
      status.set(401);

      WebforjPushException e =
          await(sender.send(subscription, PushMessage.create("Title").build()));

      assertEquals(PushStatus.REJECTED, e.getStatus());
      assertEquals(401, e.getStatusCode());
      assertTrue(e.getMessage().contains(subscription.getEndpoint()), e.getMessage());
      assertNotNull(e.getCause());
    }

    @Test
    void shouldReportAnUnreachablePushServiceWithStatusZero() throws Exception {
      server.stop(0);

      WebforjPushException e =
          await(sender.send(subscription, PushMessage.create("Title").build()));

      assertEquals(PushStatus.UNREACHABLE, e.getStatus());
      assertEquals(0, e.getStatusCode());
    }

    @Test
    void shouldReportThePushServiceThatDoesNotAnswerInTimeAsUnreachable() throws Exception {
      gate.set(new CountDownLatch(1));
      sender.setTimeout(Duration.ofMillis(300));

      WebforjPushException e =
          await(sender.send(subscription, PushMessage.create("Title").build()));

      assertEquals(PushStatus.UNREACHABLE, e.getStatus());
      assertEquals(0, e.getStatusCode());
    }

    @Test
    void shouldReportTheSubscriptionItCannotEncodeAsUnknown() throws Exception {
      PushSubscription corrupt =
          new PushSubscription(subscription.getEndpoint(), "not-a-key", subscription.getAuth());

      WebforjPushException e = await(sender.send(corrupt, PushMessage.create("Title").build()));

      assertEquals(PushStatus.UNKNOWN, e.getStatus());
      assertEquals(0, receivedBodyLength.get(), "no request reached the push service");
      assertNotNull(e.getCause());
    }

    @Test
    void shouldCutTheTimeToLiveToTheMaximum() throws Exception {
      status.set(201);
      PushMessage message = PushMessage.create("Title").setTimeToLive(Duration.ofDays(60)).build();

      assertNull(await(sender.send(subscription, message)));

      assertEquals(String.valueOf(PushSender.MAX_TIME_TO_LIVE.toSeconds()),
          receivedHeaders.get("Ttl"));
    }

    @Test
    void shouldCutTheNegativeTimeToLiveToZero() throws Exception {
      status.set(201);
      PushMessage message =
          PushMessage.create("Title").setTimeToLive(Duration.ofSeconds(-5)).build();

      assertNull(await(sender.send(subscription, message)));

      assertEquals("0", receivedHeaders.get("Ttl"));
    }

    @Test
    void shouldRequireSubscriptionAndMessage() {
      PushMessage message = PushMessage.create("Title").build();
      assertThrows(NullPointerException.class, () -> sender.send(null, message));
      assertThrows(NullPointerException.class, () -> sender.send(subscription, null));
    }
  }

  @Nested
  class Completion {

    @Test
    void shouldShareOneTransportAcrossSenders() {
      assertNotNull(PushSender.transport());
      assertSame(PushSender.transport(), PushSender.transport());
    }

    @Test
    void shouldCompleteOnTheEnvironmentOfTheCallingThread() throws Exception {
      Environment environment = mock(Environment.class);
      AtomicReference<Environment> dispatchedTo = new AtomicReference<>();
      PushSender routed = recording(dispatchedTo);
      status.set(201);

      PendingResult<Void> result;
      try (MockedStatic<Environment> mocked = mockStatic(Environment.class);
          MockedStatic<ObjectTable> table = objectTable()) {
        mocked.when(Environment::getCurrent).thenReturn(environment);
        result = routed.send(subscription, PushMessage.create("Title").build());
      }

      assertNull(await(result));
      assertSame(environment, dispatchedTo.get());
    }

    @Test
    void shouldCompleteDirectlyWithoutAnEnvironment() throws Exception {
      AtomicReference<Environment> dispatchedTo = new AtomicReference<>(mock(Environment.class));
      PushSender routed = recording(dispatchedTo);
      status.set(201);

      assertNull(await(routed.send(subscription, PushMessage.create("Title").build())));

      assertNull(dispatchedTo.get());
    }

    @Test
    void shouldDropTheCompletionWhenTheSessionEndedBeforeTheAnswer() {
      Environment environment = mock(Environment.class);
      AtomicReference<Environment> dispatchedTo = new AtomicReference<>();
      PushSender routed = recording(dispatchedTo);
      CountDownLatch answer = new CountDownLatch(1);
      gate.set(answer);
      status.set(201);

      PendingResult<Void> result;
      try (MockedStatic<Environment> mocked = mockStatic(Environment.class);
          MockedStatic<ObjectTable> table = objectTable()) {
        mocked.when(Environment::getCurrent).thenReturn(environment);
        result = routed.send(subscription, PushMessage.create("Title").build());
        Push.getCurrent().destroy();
      }

      answer.countDown();
      assertThrows(TimeoutException.class, () -> await(result, 1),
          "a session that ended never sees the completion");
      assertNull(dispatchedTo.get(), "nothing is handed to a dead environment");
      assertFalse(result.isDone());
    }

    @Test
    void shouldRunTheCompletionDirectlyWithoutAnEnvironment() {
      AtomicBoolean ran = new AtomicBoolean();

      PushSender.dispatch(null, () -> ran.set(true));

      assertTrue(ran.get());
    }

    @Test
    void shouldHandTheCompletionToTheEnvironment() {
      Environment environment = mock(Environment.class);
      AtomicReference<Runnable> handed = new AtomicReference<>();
      AtomicBoolean ran = new AtomicBoolean();

      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(() -> Environment.runLater(any(Environment.class), any(Runnable.class)))
            .thenAnswer(invocation -> {
              handed.set(invocation.getArgument(1));
              return new PendingResult<Void>();
            });

        PushSender.dispatch(environment, () -> ran.set(true));
      }

      assertFalse(ran.get(), "the completion waits for the environment");
      handed.get().run();
      assertTrue(ran.get());
    }

    @Test
    void shouldDropTheCompletionWhenTheEnvironmentCannotTakeIt() {
      Environment environment = mock(Environment.class);
      AtomicBoolean ran = new AtomicBoolean();

      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(() -> Environment.runLater(any(Environment.class), any(Runnable.class)))
            .thenReturn(PendingResult.completedExceptionallyWith(new IllegalStateException()));

        PushSender.dispatch(environment, () -> ran.set(true));
      }

      assertFalse(ran.get(), "a completion never runs on a foreign thread");
    }

    @Test
    void shouldDropTheCompletionWhenTheEnvironmentIsGone() {
      Environment environment = mock(Environment.class);
      AtomicBoolean ran = new AtomicBoolean();
      PendingResult<Void> cancelled = new PendingResult<>();
      cancelled.cancel();

      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(() -> Environment.runLater(any(Environment.class), any(Runnable.class)))
            .thenReturn(cancelled);

        PushSender.dispatch(environment, () -> ran.set(true));
      }

      assertFalse(ran.get(), "a completion never runs on a foreign thread");
    }

    private PushSender recording(AtomicReference<Environment> dispatchedTo) {
      return new PushSender(new PushConfiguration(keys(), "mailto:ops@example.com"),
          (target, completion) -> {
            dispatchedTo.set(target);
            completion.run();
          });
    }

    private MockedStatic<ObjectTable> objectTable() {
      Map<String, Object> table = new ConcurrentHashMap<>();
      MockedStatic<ObjectTable> mocked = mockStatic(ObjectTable.class);
      mocked.when(() -> ObjectTable.contains(anyString()))
          .thenAnswer(invocation -> table.containsKey(invocation.getArgument(0)));
      mocked.when(() -> ObjectTable.get(anyString()))
          .thenAnswer(invocation -> table.get(invocation.getArgument(0)));
      mocked.when(() -> ObjectTable.put(anyString(), any())).thenAnswer(invocation -> {
        Object value = invocation.getArgument(1);
        if (value == null) {
          table.remove(invocation.getArgument(0));
        } else {
          table.put(invocation.getArgument(0), value);
        }

        return value;
      });

      return mocked;
    }
  }

  private static WebforjPushException await(PendingResult<Void> result) throws Exception {
    return await(result, 5);
  }

  private static WebforjPushException await(PendingResult<Void> result, int seconds)
      throws Exception {
    CompletableFuture<WebforjPushException> outcome = new CompletableFuture<>();
    result.thenAccept(v -> outcome.complete(null));
    result.exceptionally(e -> {
      outcome.complete((WebforjPushException) e);
      return null;
    });

    return outcome.get(seconds, TimeUnit.SECONDS);
  }

  private static PushKeys keys() {
    VapidKeys generated = VapidKeys.generate();
    return new PushKeys(generated.getX509PublicKey(), generated.getPkcs8PrivateKey());
  }
}
