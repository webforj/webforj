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

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.PendingResult;
import com.webforj.bbj.security.fixture.CapturedLog;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.router.history.Location;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Stream;
import java.util.logging.Level;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InOrder;
import org.mockito.Mockito;

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
  private Config config = ConfigFactory.empty();
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
    return BBjAuthenticationService.create().setGateway(gateway).setRunner(runner).setStore(store)
        .setState(state).setRequestedLocation(requestedLocation).setNavigator(navigator)
        .setConfiguration(BBjRouteSecurityConfiguration.create(config)).build();
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

  @ParameterizedTest
  @MethodSource("createNavigationCases")
  void shouldChooseTheLocationAfterTheSignIn(String settings, String requested, String expected)
      throws Exception {
    config = ConfigFactory.parseString(settings);
    requestedLocation = () -> Optional.ofNullable(requested).map(Location::new);
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);

    getStatus(createService().startLogin("alice", PASSWORD));

    verify(navigator, timeout(5000)).accept(new Location(expected));
  }

  private static Stream<Arguments> createNavigationCases() {
    String home = "webforj.security.bbj.default-success-path = \"/home\"\n";
    String stay = "webforj.security.bbj.return-after-login = false\n";

    return Stream.of(Arguments.of(home, "/protected", "/protected"),
        Arguments.of(home, null, "/home"), Arguments.of(home + stay, "/protected", "/home"),
        Arguments.of(home + stay, null, "/home"));
  }

  @Test
  void shouldConsumeTheRequestedLocationWhenTheVisitorDoesNotReturn() throws Exception {
    config = ConfigFactory.parseString("webforj.security.bbj.return-after-login = false");
    AtomicBoolean consumed = new AtomicBoolean();
    requestedLocation = () -> {
      consumed.set(true);

      return Optional.of(new Location("/protected"));
    };
    when(gateway.openSession("alice", PASSWORD)).thenReturn(session);

    getStatus(createService().startLogin("alice", PASSWORD));

    verify(navigator, timeout(5000)).accept(new Location("/"));
    assertTrue(consumed.get());
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

  @Nested
  class Logout {
    private final Location logoutLocation = new Location("/login?logout");

    private void signIn() {
      state.setAuthentication(new BBjAuthentication("alice", Set.of(), EXPIRY));
      when(store.readSession()).thenReturn(Optional.of(session));
    }

    private void awaitResult(PendingResult<Void> result) throws Exception {
      CompletableFuture<Void> future = new CompletableFuture<>();
      result.thenAccept(future::complete);
      future.get(5, TimeUnit.SECONDS);
    }

    private BBjAdminTaskRunner createPostingRunner(AtomicReference<Runnable> posted,
        CountDownLatch postedLatch) {
      runner.destroy();
      runner = new BBjAdminTaskRunner(Duration.ofSeconds(10), () -> task -> {
        posted.set(task);
        postedLatch.countDown();

        return PendingResult.completedWith(null);
      });

      return runner;
    }

    @Test
    void shouldClearTheStateAndTheStoreBeforeNavigatingToTheLogoutLocation() throws Exception {
      signIn();
      AtomicBoolean signedInOnNavigation = new AtomicBoolean(true);
      doAnswer(invocation -> {
        signedInOnNavigation.set(state.getAuthentication().isPresent());

        return null;
      }).when(navigator).accept(any());

      awaitResult(createService().startLogout());

      InOrder order = Mockito.inOrder(store, navigator, gateway);
      order.verify(store).readSession();
      order.verify(store).clearSession();
      order.verify(navigator).accept(logoutLocation);
      order.verify(gateway).closeSession(session);
      assertFalse(signedInOnNavigation.get());
      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldEndTheStoredTokenOnAWorkerThread() throws Exception {
      signIn();
      AtomicReference<Thread> caller = new AtomicReference<>();
      doAnswer(invocation -> {
        caller.set(Thread.currentThread());

        return null;
      }).when(gateway).closeSession(any());

      awaitResult(createService().startLogout());

      verify(gateway).closeSession(session);
      assertTrue(caller.get() != Thread.currentThread());
    }

    @Test
    void shouldCompleteTheResultAfterTheServerCallEnded() throws Exception {
      signIn();
      CountDownLatch release = new CountDownLatch(1);
      CountDownLatch started = new CountDownLatch(1);
      doAnswer(invocation -> {
        started.countDown();
        release.await(5, TimeUnit.SECONDS);

        return null;
      }).when(gateway).closeSession(any());

      PendingResult<Void> result = createService().startLogout();

      assertTrue(started.await(5, TimeUnit.SECONDS));
      assertFalse(result.isDone());
      verify(navigator).accept(logoutLocation);
      assertTrue(state.getAuthentication().isEmpty());
      release.countDown();
      awaitResult(result);
    }

    @ParameterizedTest
    @EnumSource(value = BBjAdminFailure.Kind.class, names = "TOKEN_REJECTED", mode = Mode.EXCLUDE)
    void shouldCompleteWithOneWarningWhenTheServerCallFails(BBjAdminFailure.Kind kind)
        throws Exception {
      signIn();
      doThrow(new BBjAdminFailure(kind, "down")).when(gateway).closeSession(session);

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        awaitResult(createService().startLogout());

        assertEquals(List.of("The BBj login token could not be ended and expires on its own"),
            log.getMessages(Level.WARNING));
        for (String message : log.getAllMessages()) {
          assertFalse(message.contains("tok-abc-123"));
        }
      }

      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldCompleteQuietlyWhenTheTokenWasRejected() throws Exception {
      signIn();
      doThrow(new BBjAdminFailure(BBjAdminFailure.Kind.TOKEN_REJECTED, "ended")).when(gateway)
          .closeSession(session);

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        awaitResult(createService().startLogout());

        assertTrue(log.getMessages(Level.WARNING).isEmpty());
      }
    }

    @Test
    void shouldSignOutWithoutAServerCallWhenNothingIsStored() throws Exception {
      state.setAuthentication(new BBjAuthentication("alice", Set.of(), EXPIRY));
      when(store.readSession()).thenReturn(Optional.empty());

      awaitResult(createService().startLogout());

      assertTrue(state.getAuthentication().isEmpty());
      verify(store).clearSession();
      verify(navigator).accept(logoutLocation);
      verifyNoInteractions(gateway);
    }

    @Test
    void shouldNavigateAndEndTheTokenWhenTheStoreCannotBeCleared() throws Exception {
      signIn();
      doThrow(new IllegalStateException("no storage")).when(store).clearSession();

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        awaitResult(createService().startLogout());

        assertEquals(List.of("The sign-out step failed with java.lang.IllegalStateException"),
            log.getMessages(Level.WARNING));
      }

      verify(navigator).accept(logoutLocation);
      verify(gateway).closeSession(session);
      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldEndTheTokenWhenTheNavigationFails() throws Exception {
      signIn();
      doThrow(new IllegalStateException("no router")).when(navigator).accept(any());

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        awaitResult(createService().startLogout());

        assertEquals(
            List.of("Navigation after sign-out failed with java.lang.IllegalStateException"),
            log.getMessages(Level.WARNING));
      }

      verify(gateway).closeSession(session);
      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldDoNothingWhenNobodyIsSignedIn() throws Exception {
      when(store.readSession()).thenReturn(Optional.empty());
      BBjAuthenticationService service = createService();

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        PendingResult<Void> result = service.startLogout();

        assertTrue(result.isDone());
        assertTrue(log.getAllMessages().isEmpty());
      }

      verify(store).clearSession();
      verifyNoInteractions(gateway, navigator);
    }

    @Test
    void shouldCleanUpAStaleEntryWithoutNavigating() throws Exception {
      when(store.readSession()).thenReturn(Optional.of(session));

      awaitResult(createService().startLogout());

      verify(store).clearSession();
      verify(gateway).closeSession(session);
      verifyNoInteractions(navigator);
    }

    @Test
    void shouldLogOutOnceWhenCalledTwice() throws Exception {
      signIn();
      when(store.readSession()).thenReturn(Optional.of(session), Optional.empty());
      BBjAuthenticationService service = createService();

      awaitResult(service.startLogout());
      awaitResult(service.startLogout());

      verify(navigator, Mockito.times(1)).accept(any());
      verify(gateway, Mockito.times(1)).closeSession(any());
    }

    @Test
    void shouldSignOutAVisitorWhoSignedInBefore() throws Exception {
      when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
      when(store.readSession()).thenReturn(Optional.of(session));
      BBjAuthenticationService service = createService();
      getStatus(service.startLogin("alice", PASSWORD));
      assertTrue(state.getAuthentication().isPresent());

      awaitResult(service.startLogout());

      assertTrue(state.getAuthentication().isEmpty());
      verify(navigator).accept(logoutLocation);
      verify(gateway).closeSession(session);
    }

    @Test
    void shouldCancelASignInInFlightAndEndItsLateToken() throws Exception {
      AtomicReference<Runnable> posted = new AtomicReference<>();
      CountDownLatch postedLatch = new CountDownLatch(1);
      createPostingRunner(posted, postedLatch);
      when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
      when(store.readSession()).thenReturn(Optional.empty());
      BBjAuthenticationService service = createService();
      PendingResult<BBjLoginStatus> login = service.startLogin("alice", PASSWORD);
      assertTrue(postedLatch.await(5, TimeUnit.SECONDS));

      service.startLogout();
      posted.get().run();

      assertTrue(login.isCancelled());
      verify(gateway, timeout(5000)).closeSession(session);
      verify(store, never()).saveSession(any());
      verifyNoInteractions(navigator);
      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldKeepTheSignOutWhenTheResultIsCancelled() throws Exception {
      signIn();
      CountDownLatch release = new CountDownLatch(1);
      CountDownLatch started = new CountDownLatch(1);
      doAnswer(invocation -> {
        started.countDown();
        release.await(5, TimeUnit.SECONDS);

        return null;
      }).when(gateway).closeSession(any());
      PendingResult<Void> result = createService().startLogout();
      assertTrue(started.await(5, TimeUnit.SECONDS));

      result.cancel();
      release.countDown();

      verify(gateway).closeSession(session);
      verify(navigator).accept(logoutLocation);
      assertTrue(state.getAuthentication().isEmpty());
    }

    @Test
    void shouldConsumeTheRequestedLocation() throws Exception {
      signIn();
      AtomicBoolean consumed = new AtomicBoolean();
      requestedLocation = () -> {
        consumed.set(true);

        return Optional.of(new Location("/protected"));
      };

      awaitResult(createService().startLogout());

      assertTrue(consumed.get());
    }

    @Test
    void shouldNavigateToTheConfiguredLogoutLocation() throws Exception {
      config = ConfigFactory.parseString("webforj.security.bbj.logout-success-path = \"/bye\"");
      signIn();

      awaitResult(createService().startLogout());

      verify(navigator).accept(new Location("/bye"));
    }

    @Test
    void shouldNeverLogThePasswordOrTheTokenOverALoginAndALogout() throws Exception {
      when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
      when(store.readSession()).thenReturn(Optional.of(session));
      doThrow(new BBjAdminFailure(BBjAdminFailure.Kind.UNAVAILABLE, "down")).when(gateway)
          .closeSession(session);

      try (CapturedLog log = new CapturedLog(BBjAuthenticationService.class.getPackageName())) {
        BBjAuthenticationService service = createService();
        getStatus(service.startLogin("alice", PASSWORD));
        awaitResult(service.startLogout());

        for (String message : log.getAllMessages()) {
          assertFalse(message.contains(PASSWORD));
          assertFalse(message.contains("tok-abc-123"));
          assertFalse(message.contains(session.toString()));
        }
      }
    }
  }
}
