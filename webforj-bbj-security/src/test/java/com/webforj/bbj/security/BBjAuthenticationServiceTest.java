package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.fixture.CapturedLog;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.logging.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

class BBjAuthenticationServiceTest {
  private static final String PASSWORD = "s3cret-pw";
  private static final Instant EXPIRY = Instant.parse("2026-01-01T13:00:00Z");
  private final BBjLoginSession session = new BBjLoginSession("alice", "tok-abc-123", EXPIRY);
  private final BBjAdminGateway gateway = mock(BBjAdminGateway.class);
  private final BBjTokenStore store = mock(BBjTokenStore.class);
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  @SuppressWarnings("unchecked")
  private final Consumer<Location> navigator = mock(Consumer.class);
  private Supplier<Optional<Location>> requestedLocation = Optional::empty;
  private BBjAdminTaskRunner runner;

  @BeforeEach
  void setUp() {
    runner = new BBjAdminTaskRunner(Duration.ofSeconds(10), () -> task -> {
      task.run();

      return PendingResult.completedWith(null);
    });
  }

  @AfterEach
  void tearDown() {
    runner.destroy();
  }

  private BBjAuthenticationService createService() {
    return new BBjAuthenticationService(gateway, runner, store, state, requestedLocation,
        navigator);
  }

  private static BBjLoginStatus getStatus(PendingResult<BBjLoginStatus> result) throws Exception {
    CompletableFuture<BBjLoginStatus> future = new CompletableFuture<>();
    result.thenAccept(future::complete);

    return future.get(5, TimeUnit.SECONDS);
  }

  @Test
  void shouldSignInAndNavigateToTheRequestedLocation() throws Exception {
    Location target = new Location("/protected");
    requestedLocation = () -> Optional.of(target);
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);

    BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

    assertEquals(BBjLoginStatus.AUTHENTICATED, status);
    verify(store).saveSession(session);
    BBjAuthentication authentication = state.getAuthentication().orElseThrow();
    assertEquals("alice", authentication.getUserName());
    assertTrue(authentication.getRoles().isEmpty());
    assertEquals(EXPIRY, authentication.getExpiresAt());
    verify(navigator, timeout(5000)).accept(target);
  }

  @Test
  void shouldNavigateToTheRootWithoutARequestedLocation() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);

    getStatus(createService().startLogin("alice", PASSWORD));

    verify(navigator, timeout(5000)).accept(new Location("/"));
  }

  @Test
  void shouldFillTheStateBeforeNavigating() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    AtomicBoolean signedInOnNavigation = new AtomicBoolean();
    CountDownLatch navigated = new CountDownLatch(1);
    doAnswer(invocation -> {
      signedInOnNavigation.set(state.getAuthentication().isPresent());
      navigated.countDown();

      return null;
    }).when(navigator).accept(any());

    getStatus(createService().startLogin("alice", PASSWORD));

    assertTrue(navigated.await(5, TimeUnit.SECONDS));
    assertTrue(signedInOnNavigation.get());
  }

  @Test
  void shouldAnswerInvalidCredentialsForARefusedPassword() throws Exception {
    when(gateway.openSession("alice", PASSWORD))
        .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, status);
      assertTrue(log.getMessages(Level.WARNING).isEmpty());
    }

    verifyNoInteractions(store, navigator);
    assertTrue(state.getAuthentication().isEmpty());
  }

  @ParameterizedTest
  @CsvSource({"'',pw", "' ',pw", "alice,''", "alice,' '"})
  void shouldAnswerInvalidCredentialsForBlankInputWithoutAServerCall(String userName,
      String password) throws Exception {
    assertEquals(BBjLoginStatus.INVALID_CREDENTIALS,
        getStatus(createService().startLogin(userName, password)));

    verifyNoInteractions(gateway, store, navigator);
  }

  @Test
  void shouldRejectNullInput() {
    BBjAuthenticationService service = createService();

    assertThrows(NullPointerException.class, () -> service.startLogin(null, PASSWORD));
    assertThrows(NullPointerException.class, () -> service.startLogin("alice", null));
  }

  @ParameterizedTest
  @EnumSource(value = BBjAdminFailure.Kind.class, names = {"UNAVAILABLE", "TOKEN_REJECTED"})
  void shouldAnswerUnavailableWithOneWarning(BBjAdminFailure.Kind kind) throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenThrow(new BBjAdminFailure(kind, "down"));

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

      assertEquals(BBjLoginStatus.UNAVAILABLE, status);
      assertEquals(List.of("The BBj Admin API could not complete the sign-in"),
          log.getMessages(Level.WARNING));
    }

    verifyNoInteractions(store, navigator);
  }

  @Test
  void shouldAnswerUnavailableAndSayWhyForATwoFactorAccount() throws Exception {
    when(gateway.openSession("alice", PASSWORD))
        .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.TWO_FACTOR_REQUIRED, "code needed"));

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

      assertEquals(BBjLoginStatus.UNAVAILABLE, status);
      assertEquals(
          List.of(
              "Two-factor authentication is required for this account and is not supported yet"),
          log.getMessages(Level.WARNING));
    }
  }

  @Test
  void shouldRetireTheSessionWhenItCannotBeKept() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    doThrow(new IllegalStateException("not kept")).when(store).saveSession(session);

    BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    verify(gateway, timeout(5000)).closeSession(session);
    assertTrue(state.getAuthentication().isEmpty());
    verifyNoInteractions(navigator);
  }

  @Test
  void shouldLogWhenAnUnkeptSessionCannotBeClosed() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    doThrow(new IllegalStateException("not kept")).when(store).saveSession(session);
    doThrow(new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE, "down")).when(gateway)
        .closeSession(session);

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      getStatus(createService().startLogin("alice", PASSWORD));
      assertTrue(log.awaitMessage(Level.WARNING,
          "A session that was not kept could not be closed on the server", 5, TimeUnit.SECONDS));
    }
  }

  @Test
  void shouldKeepTheSignInWhenTheNavigationFails() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    doThrow(new IllegalStateException("no router")).when(navigator).accept(any());

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      BBjLoginStatus status = getStatus(createService().startLogin("alice", PASSWORD));

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
      assertTrue(log.awaitMessage(Level.WARNING,
          "Navigation after sign-in failed with java.lang.IllegalStateException", 5,
          TimeUnit.SECONDS));
      assertEquals(List.of("Navigation after sign-in failed with java.lang.IllegalStateException"),
          log.getMessages(Level.WARNING));
    }
  }

  @Test
  void shouldKeepTheSignInWhenTheNavigationCancelsTheResult() throws Exception {
    AtomicReference<Runnable> posted = new AtomicReference<>();
    CountDownLatch postedLatch = new CountDownLatch(1);
    runner.destroy();
    runner = new BBjAdminTaskRunner(Duration.ofSeconds(10), () -> task -> {
      posted.set(task);
      postedLatch.countDown();

      return PendingResult.completedWith(null);
    });
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    PendingResult<BBjLoginStatus> result = createService().startLogin("alice", PASSWORD);
    doAnswer(invocation -> {
      result.cancel();

      return null;
    }).when(navigator).accept(any());
    assertTrue(postedLatch.await(5, TimeUnit.SECONDS));

    posted.get().run();

    assertEquals(BBjLoginStatus.AUTHENTICATED, getStatus(result));
    verify(gateway, never()).closeSession(any());
    assertTrue(state.getAuthentication().isPresent());
  }

  @Test
  void shouldRetireTheSessionOfACancelledSignIn() throws Exception {
    AtomicReference<Runnable> posted = new AtomicReference<>();
    CountDownLatch postedLatch = new CountDownLatch(1);
    runner.destroy();
    runner = new BBjAdminTaskRunner(Duration.ofSeconds(10), () -> task -> {
      posted.set(task);
      postedLatch.countDown();

      return PendingResult.completedWith(null);
    });
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);

    PendingResult<BBjLoginStatus> result = createService().startLogin("alice", PASSWORD);
    assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
    result.cancel();
    posted.get().run();

    verify(gateway, timeout(5000)).closeSession(session);
    verify(store, never()).saveSession(any());
    verifyNoInteractions(navigator);
    assertTrue(state.getAuthentication().isEmpty());
  }

  @Test
  void shouldLogWhenALateSessionCannotBeClosed() throws Exception {
    AtomicReference<Runnable> posted = new AtomicReference<>();
    CountDownLatch postedLatch = new CountDownLatch(1);
    runner.destroy();
    runner = new BBjAdminTaskRunner(Duration.ofSeconds(10), () -> task -> {
      posted.set(task);
      postedLatch.countDown();

      return PendingResult.completedWith(null);
    });
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    CountDownLatch closed = new CountDownLatch(1);
    doAnswer(invocation -> {
      closed.countDown();

      throw new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE, "down");
    }).when(gateway).closeSession(session);

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      PendingResult<BBjLoginStatus> result = createService().startLogin("alice", PASSWORD);
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));
      result.cancel();
      posted.get().run();
      assertTrue(closed.await(5, TimeUnit.SECONDS));
      assertTrue(log.awaitMessage(Level.WARNING,
          "A session nobody took could not be closed on the server", 5, TimeUnit.SECONDS));

      assertEquals(List.of("A session nobody took could not be closed on the server"),
          log.getMessages(Level.WARNING));
    }
  }

  @Test
  void shouldNeverLogThePasswordOrTheToken() throws Exception {
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
    doThrow(new IllegalStateException(PASSWORD)).when(store).saveSession(session);

    try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
      getStatus(createService().startLogin("alice", PASSWORD));
      verify(gateway, timeout(5000)).closeSession(session);

      for (String message : log.getAllMessages()) {
        assertFalse(message.contains(PASSWORD));
        assertFalse(message.contains("tok-abc-123"));
        assertFalse(message.contains(session.toString()));
      }
    }
  }
}
