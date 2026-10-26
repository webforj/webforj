package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

class BBjAuthenticationServiceTest {
  private static final String PASSWORD = "s3cret-pw";
  private static final Location DEFAULT_LOCATION = new Location("/home");
  private static final Location REQUESTED_LOCATION = new Location("/orders");
  private final BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
  private final BBjSession session =
      new BBjSession(principal, "tok-abc-123", Instant.parse("2026-01-01T13:00:00Z"));
  private final BBjAdminGateway gateway = mock(BBjAdminGateway.class);
  private final BBjAdminWorker worker = mock(BBjAdminWorker.class);
  private final BBjRequestedLocationStore locationStore = mock(BBjRequestedLocationStore.class);
  private final BBjSecurityConfigurationProperties properties =
      new BBjSecurityConfigurationProperties();
  private final BBjRouteSecurityConfiguration configuration =
      new BBjRouteSecurityConfiguration(properties);
  private final Consumer<Location> navigator = createNavigator();
  private final PendingResult<BBjLoginStatus> pending = new PendingResult<>();
  private final ArgumentCaptor<Supplier<BBjSession>> workCaptor = createCaptor(Supplier.class);
  private final ArgumentCaptor<Function<BBjSession, BBjLoginStatus>> deliverCaptor =
      createCaptor(Function.class);
  private final ArgumentCaptor<Function<BBjAdminFailure, BBjLoginStatus>> failCaptor =
      createCaptor(Function.class);
  private final ArgumentCaptor<Consumer<BBjSession>> discardCaptor = createCaptor(Consumer.class);
  private final List<LogRecord> records = new CopyOnWriteArrayList<>();
  private final Logger logger = Logger.getLogger(BBjAuthenticationService.class.getName());
  private final Handler handler = new Handler() {
    @Override
    public void publish(LogRecord logRecord) {
      records.add(logRecord);
    }

    @Override
    public void flush() {
      // Not implemented
    }

    @Override
    public void close() {
      // Not implemented
    }
  };
  private Level previousLevel;
  private BBjAuthenticationService service;

  @BeforeEach
  void setUp() {
    previousLevel = logger.getLevel();
    logger.setLevel(Level.ALL);
    logger.addHandler(handler);
    properties.setDefaultSuccessPath("/home");
    doReturn(pending).when(worker).start(any(), any(), any(), any());
    service = new BBjAuthenticationService(gateway, worker, locationStore, properties,
        configuration, navigator);
  }

  @AfterEach
  void tearDown() {
    logger.removeHandler(handler);
    logger.setLevel(previousLevel);
  }

  static Stream<Arguments> getRefusedInputs() {
    return Stream.of(Arguments.of("", PASSWORD), Arguments.of("  ", PASSWORD),
        Arguments.of("alice", ""), Arguments.of("alice", "  "),
        Arguments.of("a".repeat(257), PASSWORD), Arguments.of("alice", "p".repeat(1025)));
  }

  static Stream<Arguments> getAcceptedInputs() {
    return Stream.of(Arguments.of("a".repeat(256), PASSWORD),
        Arguments.of("alice", "p".repeat(1024)));
  }

  static Stream<Arguments> getLogoutCases() {
    return Stream.of(Arguments.of("/goodbye", new Location("/goodbye")),
        Arguments.of(null, new Location("/login")));
  }

  static Stream<Arguments> getNavigationCases() {
    return Stream.of(Arguments.of(true, Optional.of(REQUESTED_LOCATION), REQUESTED_LOCATION),
        Arguments.of(true, Optional.empty(), DEFAULT_LOCATION),
        Arguments.of(false, Optional.of(REQUESTED_LOCATION), DEFAULT_LOCATION));
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private static <T> ArgumentCaptor<T> createCaptor(Class<?> type) {
    return ArgumentCaptor.forClass((Class) type);
  }

  @SuppressWarnings("unchecked")
  private static Consumer<Location> createNavigator() {
    return mock(Consumer.class);
  }

  private static BBjLoginStatus getStatus(PendingResult<BBjLoginStatus> result) throws Exception {
    CompletableFuture<BBjLoginStatus> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  private static <T> T getValue(PendingResult<T> result) throws Exception {
    CompletableFuture<T> future = new CompletableFuture<>();
    result.thenAccept(future::complete).exceptionally(error -> {
      future.completeExceptionally(error);

      return null;
    });

    return future.get(5, TimeUnit.SECONDS);
  }

  private void captureHooks() {
    verify(worker).start(workCaptor.capture(), deliverCaptor.capture(), failCaptor.capture(),
        discardCaptor.capture());
  }

  private List<String> getWarnings() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.WARNING)
        .map(formatter::formatMessage).toList();
  }

  private List<String> getDebugMessages() {
    SimpleFormatter formatter = new SimpleFormatter();

    return records.stream().filter(it -> it.getLevel() == Level.FINE).map(formatter::formatMessage)
        .toList();
  }

  @Nested
  class Construction {

    @ParameterizedTest
    @ValueSource(ints = {0, 1, 2, 3, 4, 5})
    void shouldRefuseNullArgument(int position) {
      BBjAdminGateway givenGateway = position == 0 ? null : gateway;
      BBjAdminWorker givenWorker = position == 1 ? null : worker;
      BBjRequestedLocationStore givenStore = position == 2 ? null : locationStore;
      BBjSecurityConfigurationProperties givenProperties = position == 3 ? null : properties;
      BBjRouteSecurityConfiguration givenConfiguration = position == 4 ? null : configuration;
      Consumer<Location> givenNavigator = position == 5 ? null : navigator;

      assertThrows(NullPointerException.class, () -> new BBjAuthenticationService(givenGateway,
          givenWorker, givenStore, givenProperties, givenConfiguration, givenNavigator));
    }
  }

  @Nested
  class StartLoginInput {

    @Test
    void shouldRefuseNullUserName() {
      assertThrows(NullPointerException.class, () -> service.startLogin(null, PASSWORD));
    }

    @Test
    void shouldRefuseNullPassword() {
      assertThrows(NullPointerException.class, () -> service.startLogin("alice", null));
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAuthenticationServiceTest#getRefusedInputs")
    void shouldCompleteWithInvalidCredentialsWithoutCallingAnyone(String userName, String password)
        throws Exception {
      PendingResult<BBjLoginStatus> result = service.startLogin(userName, password);

      assertTrue(result.isDone());
      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, getStatus(result));
      verifyNoInteractions(worker, gateway);
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAuthenticationServiceTest#getAcceptedInputs")
    void shouldAcceptInputAtTheLimit(String userName, String password) {
      service.startLogin(userName, password);

      captureHooks();
    }
  }

  @Nested
  class Call {

    @Test
    void shouldHandTheWorkerFourHooks() {
      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);

      captureHooks();
      assertSame(pending, result);
      assertNotNull(workCaptor.getValue());
      assertNotNull(deliverCaptor.getValue());
      assertNotNull(failCaptor.getValue());
      assertNotNull(discardCaptor.getValue());
    }

    @Test
    void shouldCancelTheRunningSignInWhenSecondOneStarts() {
      PendingResult<BBjLoginStatus> second = new PendingResult<>();
      doReturn(pending).doReturn(second).when(worker).start(any(), any(), any(), any());
      service.startLogin("alice", PASSWORD);

      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);

      assertTrue(pending.isCancelled());
      assertSame(second, result);
      assertFalse(second.isCancelled());
    }

    @Test
    void shouldOpenTheSessionInTheWork() {
      when(gateway.openSession("alice", PASSWORD)).thenReturn(session);
      service.startLogin("alice", PASSWORD);

      captureHooks();

      assertSame(session, workCaptor.getValue().get());
    }
  }

  @Nested
  class DeliveredSession {

    @Test
    void shouldAuthenticateAndExposeThePrincipal() throws Exception {
      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);
      captureHooks();

      pending.complete(deliverCaptor.getValue().apply(session));

      assertEquals(BBjLoginStatus.AUTHENTICATED, getStatus(result));
      assertSame(principal, service.currentUser().orElseThrow());
    }

    @Test
    void shouldCloseTheTokenOfTheSessionItReplaces() {
      PendingResult<BBjLoginStatus> second = new PendingResult<>();
      doReturn(pending).doReturn(second).doReturn(new PendingResult<Void>()).when(worker)
          .start(any(), any(), any(), any());
      service.startLogin("alice", PASSWORD);
      captureHooks();
      pending.complete(deliverCaptor.getValue().apply(session));
      clearInvocations(worker);
      service.startLogin("alice", PASSWORD);
      captureHooks();
      clearInvocations(worker);
      BBjSession replacing =
          new BBjSession(principal, "tok-def-456", Instant.parse("2026-01-01T14:00:00Z"));

      second.complete(deliverCaptor.getValue().apply(replacing));

      verify(worker).start(workCaptor.capture(), any(), any(), any());
      workCaptor.getValue().get();
      verify(gateway).closeSession(session);
      verifyNoMoreInteractions(gateway);
      assertSame(replacing.getPrincipal(), service.currentUser().orElseThrow());
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAuthenticationServiceTest#getNavigationCases")
    void shouldReadClearAndNavigate(boolean returnAfterLogin, Optional<Location> saved,
        Location expected) {
      properties.setReturnAfterLogin(returnAfterLogin);
      when(locationStore.readLocation()).thenReturn(saved);
      service.startLogin("alice", PASSWORD);
      captureHooks();

      pending.complete(deliverCaptor.getValue().apply(session));

      verify(locationStore).readLocation();
      verify(locationStore).clearLocation();
      verify(navigator).accept(expected);
    }
  }

  @Nested
  class FailureKinds {

    @Test
    void shouldGiveInvalidCredentialsWithoutWarning() throws Exception {
      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);
      captureHooks();
      BBjAdminFailure failure = new BBjAdminFailure(Kind.INVALID_CREDENTIALS, "refused");

      pending.complete(failCaptor.getValue().apply(failure));

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, getStatus(result));
      assertEquals(List.of(), getWarnings());
    }

    @ParameterizedTest
    @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "INVALID_CREDENTIALS")
    void shouldGiveUnavailableWithOneWarning(Kind kind) throws Exception {
      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);
      captureHooks();
      BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

      pending.complete(failCaptor.getValue().apply(failure));

      assertEquals(BBjLoginStatus.UNAVAILABLE, getStatus(result));
      assertEquals(1, getWarnings().size());
    }

    @ParameterizedTest
    @EnumSource(value = Kind.class, names = {"TOKEN_REJECTED", "UNAVAILABLE"})
    void shouldLogTheFailureMessageWhenTheSignInCouldNotComplete(Kind kind) {
      service.startLogin("alice", PASSWORD);
      captureHooks();
      BBjAdminFailure failure = new BBjAdminFailure(kind, "NoClassDefFoundError: Factory");

      pending.complete(failCaptor.getValue().apply(failure));

      assertEquals(
          List.of(
              "The BBj Admin API could not complete the sign in: NoClassDefFoundError: Factory"),
          getWarnings());
    }

    @ParameterizedTest
    @EnumSource(Kind.class)
    void shouldKeepNoSessionAndNotNavigate(Kind kind) {
      service.startLogin("alice", PASSWORD);
      captureHooks();
      BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

      pending.complete(failCaptor.getValue().apply(failure));

      assertFalse(service.currentUser().isPresent());
      verifyNoInteractions(navigator);
    }

    @Test
    void shouldGiveUnavailableWhenTheWorkerRefuses() throws Exception {
      doReturn(PendingResult.completedWith(BBjLoginStatus.UNAVAILABLE)).when(worker).start(any(),
          any(), any(), any());

      PendingResult<BBjLoginStatus> result = service.startLogin("alice", PASSWORD);

      assertEquals(BBjLoginStatus.UNAVAILABLE, getStatus(result));
      assertFalse(service.currentUser().isPresent());
    }
  }

  @Nested
  class Logout {
    private final PendingResult<Void> closePending = new PendingResult<>();
    private final ArgumentCaptor<Supplier<Boolean>> closeWorkCaptor = createCaptor(Supplier.class);
    private final ArgumentCaptor<Function<Boolean, Void>> closeDeliverCaptor =
        createCaptor(Function.class);
    private final ArgumentCaptor<Function<BBjAdminFailure, Void>> closeFailCaptor =
        createCaptor(Function.class);
    private final ArgumentCaptor<Consumer<Boolean>> closeDiscardCaptor =
        createCaptor(Consumer.class);

    private void startSignedInSession() {
      service.startLogin("alice", PASSWORD);
      captureHooks();
      pending.complete(deliverCaptor.getValue().apply(session));
      clearInvocations(worker, navigator, locationStore, gateway);
      doReturn(closePending).when(worker).start(any(), any(), any(), any());
    }

    private void readCloseHooks() {
      verify(worker).start(closeWorkCaptor.capture(), closeDeliverCaptor.capture(),
          closeFailCaptor.capture(), closeDiscardCaptor.capture());
    }

    @Test
    void shouldCompleteWithNullAndCallNoOneWhenNobodySignedIn() throws Exception {
      PendingResult<Void> result = service.startLogout();

      assertTrue(result.isDone());
      assertNull(getValue(result));
      verifyNoInteractions(navigator, worker, gateway, locationStore);
    }

    @Test
    void shouldCancelTheRunningSignInAndNeverKeepItsSession() {
      service.startLogin("alice", PASSWORD);

      PendingResult<Void> result = service.startLogout();

      assertTrue(pending.isCancelled());
      assertFalse(pending.complete(BBjLoginStatus.AUTHENTICATED));
      assertTrue(result.isDone());
      assertFalse(service.currentUser().isPresent());
      verifyNoInteractions(navigator);
    }

    @Test
    void shouldForgetTheUserRightAway() {
      startSignedInSession();

      service.startLogout();

      assertFalse(service.currentUser().isPresent());
    }

    @Test
    void shouldClearTheLocationStore() {
      startSignedInSession();

      service.startLogout();

      verify(locationStore).clearLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.BBjAuthenticationServiceTest#getLogoutCases")
    void shouldOpenTheLogoutLocationOfTheConfiguration(String logoutPath, Location expected) {
      properties.setLogoutSuccessPath(logoutPath);
      startSignedInSession();

      service.startLogout();

      verify(navigator).accept(expected);
    }

    @Test
    void shouldCloseTheSignedInSession() {
      startSignedInSession();
      service.startLogout();
      readCloseHooks();

      closeWorkCaptor.getValue().get();

      verify(gateway).closeSession(session);
    }

    @Test
    void shouldCompleteWithNullAfterTheClose() throws Exception {
      startSignedInSession();
      PendingResult<Void> result = service.startLogout();
      readCloseHooks();
      closeWorkCaptor.getValue().get();
      assertFalse(result.isDone());

      closePending.complete(closeDeliverCaptor.getValue().apply(Boolean.TRUE));

      assertSame(closePending, result);
      assertNull(getValue(result));
    }

    @Test
    void shouldCompleteWithNullAndLogDebugWhenTheTokenWasRejected() throws Exception {
      startSignedInSession();
      PendingResult<Void> result = service.startLogout();
      readCloseHooks();
      BBjAdminFailure failure = new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");

      closePending.complete(closeFailCaptor.getValue().apply(failure));

      assertNull(getValue(result));
      assertEquals(List.of("The token was already ended: ended"), getDebugMessages());
      assertEquals(List.of(), getWarnings());
    }

    @ParameterizedTest
    @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
    void shouldCompleteWithNullAndWarnWhenTheTokenCouldNotBeEnded(Kind kind) throws Exception {
      startSignedInSession();
      PendingResult<Void> result = service.startLogout();
      readCloseHooks();
      BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

      closePending.complete(closeFailCaptor.getValue().apply(failure));

      assertNull(getValue(result));
      assertEquals(List.of("The token could not be ended and expires on its own: not served"),
          getWarnings());
      assertEquals(List.of(), getDebugMessages());
    }

    @Test
    void shouldActLikeNobodySignedInOnSecondLogout() throws Exception {
      startSignedInSession();
      service.startLogout();
      clearInvocations(worker, navigator, locationStore, gateway);

      PendingResult<Void> result = service.startLogout();

      assertTrue(result.isDone());
      assertNull(getValue(result));
      verifyNoInteractions(navigator, worker, gateway, locationStore);
    }
  }

  @Nested
  class CurrentUser {

    @Test
    void shouldBeEmptyBeforeAnySignIn() {
      assertFalse(service.currentUser().isPresent());
    }

    @Test
    void shouldNeverKeepDiscardedSession() {
      service.startLogin("alice", PASSWORD);
      captureHooks();

      discardCaptor.getValue().accept(session);

      assertFalse(service.currentUser().isPresent());
      verifyNoInteractions(navigator);
    }
  }

  @Nested
  class Secrets {

    @ParameterizedTest
    @EnumSource(Kind.class)
    void shouldKeepThePasswordOutOfEveryLogRecord(Kind kind) {
      service.startLogin("alice", PASSWORD);
      captureHooks();
      BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

      pending.complete(failCaptor.getValue().apply(failure));
      discardCaptor.getValue().accept(session);

      SimpleFormatter formatter = new SimpleFormatter();
      assertTrue(records.stream().noneMatch(it -> formatter.format(it).contains(PASSWORD)));
    }
  }
}
