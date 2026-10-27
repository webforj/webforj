package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static com.webforj.bbj.security.SignInFixture.createCaptor;
import static com.webforj.bbj.security.SignInFixture.getValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.logging.SimpleFormatter;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;

class BBjAuthenticationServiceCheckTest {
  private static final Instant START = Instant.parse("2026-01-01T12:00:00Z");
  private final SignInFixture fixture = new SignInFixture();
  private final BBjPrincipal renewedPrincipal =
      new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
  private final BBjSession renewed =
      new BBjSession(renewedPrincipal, "tok-new-456", Instant.parse("2026-01-01T14:00:00Z"));
  private final PendingResult<Void> checkPending = new PendingResult<>();
  private final AtomicReference<Instant> now = new AtomicReference<>(START);
  private final Clock clock = new Clock() {
    @Override
    public ZoneId getZone() {
      return ZoneOffset.UTC;
    }

    @Override
    public Clock withZone(ZoneId zone) {
      return this;
    }

    @Override
    public Instant instant() {
      return now.get();
    }
  };
  private final ArgumentCaptor<Function<BBjSession, Void>> checkDeliverCaptor =
      createCaptor(Function.class);
  private final ArgumentCaptor<Function<BBjAdminFailure, Void>> checkFailCaptor =
      createCaptor(Function.class);

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @BeforeEach
  void setUpCheck() {
    fixture.properties.setCheckSeconds(30);
    fixture.properties.setRenewBeforeMinutes(5);
    doReturn(checkPending).when(fixture.worker).start(any(), any(), any(), any());
    when(fixture.sessionStore.readSession()).thenReturn(Optional.of(fixture.session));
    fixture.service = fixture.createBuilder().setClock(clock).build();
    fixture.service.restoreSession();
  }

  private void captureCheckHooks() {
    verify(fixture.worker).start(fixture.workCaptor.capture(), checkDeliverCaptor.capture(),
        checkFailCaptor.capture(), fixture.discardCaptor.capture());
  }

  private void startCheckAndDeliver(BBjSession outcome) {
    fixture.service.startCheck();
    captureCheckHooks();
    checkPending.complete(checkDeliverCaptor.getValue().apply(outcome));
    clearInvocations(fixture.worker, fixture.gateway, fixture.sessionStore);
  }

  private void closeLastRenewedToken() {
    ArgumentCaptor<Supplier<Boolean>> closeWorkCaptor = createCaptor(Supplier.class);
    verify(fixture.worker, atLeastOnce()).start(closeWorkCaptor.capture(), any(), any(), any());
    closeWorkCaptor.getValue().get();
  }

  private List<String> getInfoMessages() {
    SimpleFormatter formatter = new SimpleFormatter();

    return fixture.records.stream().filter(it -> it.getLevel() == Level.INFO)
        .map(formatter::formatMessage).toList();
  }

  static Stream<Arguments> getWindowCases() {
    return Stream.of(Arguments.of("2026-01-01T12:54:59Z", false),
        Arguments.of("2026-01-01T12:55:00Z", true), Arguments.of("2026-01-01T12:59:59Z", true));
  }

  @Test
  void shouldCompleteAtOnceAndCallNoOneWhenNobodySignedIn() throws Exception {
    BBjSessionStore emptyStore = mock(BBjSessionStore.class);
    when(emptyStore.readSession()).thenReturn(Optional.empty());
    fixture.service = fixture.createBuilder().setSessionStore(emptyStore).setClock(clock).build();
    fixture.service.restoreSession();

    PendingResult<Void> result = fixture.service.startCheck();

    assertTrue(result.isDone());
    assertNull(getValue(result));
    verifyNoInteractions(fixture.worker, fixture.gateway);
  }

  @ParameterizedTest
  @ValueSource(strings = {"2026-01-01T13:00:00Z", "2026-01-01T13:00:01Z"})
  void shouldClearAnExpiredSessionWithNoServerCall(String time) throws Exception {
    now.set(Instant.parse(time));

    PendingResult<Void> result = fixture.service.startCheck();

    assertTrue(result.isDone());
    assertNull(getValue(result));
    assertFalse(fixture.service.currentUser().isPresent());
    verify(fixture.sessionStore).clearSession();
    verifyNoInteractions(fixture.worker, fixture.gateway);
    assertEquals(List.of("The sign in expired and was cleared"), fixture.getDebugMessages());
  }

  @Test
  void shouldReturnTheRunningCheckToEveryCaller() {
    PendingResult<Void> first = fixture.service.startCheck();
    PendingResult<Void> second = fixture.service.startCheck();

    assertSame(first, second);
    assertFalse(second.isDone());
    captureCheckHooks();
  }

  @Test
  void shouldStartCheckWhenNothingWasCheckedSinceTheRestore() {
    PendingResult<Void> result = fixture.service.startCheck();

    assertSame(checkPending, result);
    captureCheckHooks();
  }

  @Test
  void shouldAskForTheUserAndKeepTheSessionOutsideTheRenewWindow() {
    doReturn("alice").when(fixture.gateway).readUserName(fixture.session);
    fixture.service.startCheck();
    captureCheckHooks();

    BBjSession outcome = fixture.workCaptor.getValue().get();

    assertSame(fixture.session, outcome);
    verify(fixture.gateway).readUserName(fixture.session);
    verify(fixture.gateway, never()).renewSession(any());
  }

  @Test
  void shouldEndWithTokenRejectedWhenTheServerKnowsAnotherUser() {
    doReturn("bob").when(fixture.gateway).readUserName(fixture.session);
    fixture.service.startCheck();
    captureCheckHooks();
    Supplier<BBjSession> work = fixture.workCaptor.getValue();

    BBjAdminFailure failure = assertThrows(BBjAdminFailure.class, work::get);

    assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
    verify(fixture.gateway, never()).renewSession(any());
  }

  @Test
  void shouldRenewInsideTheRenewWindow() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    doReturn("alice").when(fixture.gateway).readUserName(fixture.session);
    doReturn(renewed).when(fixture.gateway).renewSession(fixture.session);
    fixture.service.startCheck();
    captureCheckHooks();

    BBjSession outcome = fixture.workCaptor.getValue().get();

    assertSame(renewed, outcome);
  }

  @Test
  void shouldNeverRenewWhenRenewBeforeMinutesIsZero() {
    fixture.properties.setRenewBeforeMinutes(0);
    now.set(Instant.parse("2026-01-01T12:59:59Z"));
    doReturn("alice").when(fixture.gateway).readUserName(fixture.session);
    fixture.service.startCheck();
    captureCheckHooks();

    BBjSession outcome = fixture.workCaptor.getValue().get();

    assertSame(fixture.session, outcome);
    verify(fixture.gateway, never()).renewSession(any());
  }

  @Test
  void shouldCompleteAtOnceWhenCheckedWithinCheckSeconds() throws Exception {
    startCheckAndDeliver(fixture.session);
    now.set(START.plusSeconds(29));

    PendingResult<Void> result = fixture.service.startCheck();

    assertTrue(result.isDone());
    assertNull(getValue(result));
    verifyNoInteractions(fixture.worker, fixture.gateway);
  }

  @Test
  void shouldStartCheckWhenCheckSecondsPassed() {
    startCheckAndDeliver(fixture.session);
    now.set(START.plusSeconds(30));

    fixture.service.startCheck();

    verify(fixture.worker).start(any(), any(), any(), any());
  }

  @Test
  void shouldStartCheckAtEveryCallWhenCheckSecondsIsZero() {
    fixture.properties.setCheckSeconds(0);
    startCheckAndDeliver(fixture.session);

    fixture.service.startCheck();

    verify(fixture.worker).start(any(), any(), any(), any());
  }

  @ParameterizedTest
  @MethodSource("getWindowCases")
  void shouldStartCheckInsideTheRenewWindowWhateverCheckSecondsSay(String time, boolean due) {
    fixture.properties.setCheckSeconds(3600);
    startCheckAndDeliver(fixture.session);
    now.set(Instant.parse(time));

    fixture.service.startCheck();

    verify(fixture.worker, due ? times(1) : never()).start(any(), any(), any(), any());
  }

  @Test
  void shouldTreatTheSignInAsTheLastCheck() {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));
    clearInvocations(fixture.worker, fixture.gateway);
    now.set(START.plusSeconds(29));

    fixture.service.startCheck();

    verifyNoInteractions(fixture.worker, fixture.gateway);
  }

  @Test
  void shouldKeepTheSessionAndLogDebugWhenTheCheckFoundNoChange() throws Exception {
    PendingResult<Void> result = fixture.service.startCheck();
    captureCheckHooks();

    checkPending.complete(checkDeliverCaptor.getValue().apply(fixture.session));

    assertNull(getValue(result));
    assertSame(fixture.principal, fixture.service.currentUser().orElseThrow());
    verify(fixture.sessionStore, never()).saveSession(any());
    assertEquals(List.of("The sign in was checked and stays"), fixture.getDebugMessages());
  }

  @Test
  void shouldSaveAndKeepTheRenewedSession() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.service.startCheck();
    captureCheckHooks();

    checkPending.complete(checkDeliverCaptor.getValue().apply(renewed));

    verify(fixture.sessionStore).saveSession(renewed);
    assertSame(renewedPrincipal, fixture.service.currentUser().orElseThrow());
    assertEquals(List.of("The sign in was renewed"), getInfoMessages());
  }

  @Test
  void shouldTreatTheRenewalAsTheLastCheck() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.properties.setCheckSeconds(3600);
    startCheckAndDeliver(renewed);
    now.set(Instant.parse("2026-01-01T12:56:00Z"));

    fixture.service.startCheck();

    verify(fixture.worker, never()).start(any(), any(), any(), any());
  }

  @Test
  void shouldSignOutAndCloseTheRenewedTokenWhenItCannotBeSaved() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    doThrow(new IllegalStateException("not kept")).when(fixture.sessionStore).saveSession(renewed);
    fixture.service.startCheck();
    captureCheckHooks();

    checkPending.complete(checkDeliverCaptor.getValue().apply(renewed));

    assertFalse(fixture.service.currentUser().isPresent());
    verify(fixture.sessionStore).clearSession();
    closeLastRenewedToken();
    verify(fixture.gateway).closeSession(renewed);
    assertEquals(List.of("The renewed sign in could not be kept, the user was signed out"),
        fixture.getWarnings());
  }

  @Test
  void shouldDropStaleRenewedSessionAndCloseItsToken() {
    fixture.service.startCheck();
    captureCheckHooks();
    fixture.service.startLogout();
    clearInvocations(fixture.sessionStore, fixture.gateway);

    checkPending.complete(checkDeliverCaptor.getValue().apply(renewed));

    assertFalse(fixture.service.currentUser().isPresent());
    verify(fixture.sessionStore, never()).saveSession(any());
    closeLastRenewedToken();
    verify(fixture.gateway).closeSession(renewed);
  }

  @Test
  void shouldDropStaleResultWithNoRenewalWithoutClosingAnything() {
    fixture.service.startCheck();
    captureCheckHooks();
    fixture.service.startLogout();
    clearInvocations(fixture.worker, fixture.sessionStore, fixture.gateway);

    checkPending.complete(checkDeliverCaptor.getValue().apply(fixture.session));

    verifyNoInteractions(fixture.worker, fixture.sessionStore, fixture.gateway);
    assertEquals(List.of(), fixture.getDebugMessages());
  }

  @Test
  void shouldSignOutAndLogInfoWhenTheTokenWasRejected() {
    fixture.service.startCheck();
    captureCheckHooks();
    BBjAdminFailure failure = new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");

    checkPending.complete(checkFailCaptor.getValue().apply(failure));

    assertFalse(fixture.service.currentUser().isPresent());
    verify(fixture.sessionStore).clearSession();
    assertEquals(List.of("The server no longer accepts the sign in, the user was signed out"),
        getInfoMessages());
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldKeepTheSignInAndWarnForEveryOtherFailure(Kind kind) {
    fixture.service.startCheck();
    captureCheckHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    checkPending.complete(checkFailCaptor.getValue().apply(failure));

    assertSame(fixture.principal, fixture.service.currentUser().orElseThrow());
    verify(fixture.sessionStore, never()).clearSession();
    assertEquals(
        List.of("The sign in could not be checked and stays until it expires: " + "not served"),
        fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldWaitCheckSecondsAfterEveryOtherFailureOutsideTheRenewWindow(Kind kind) {
    fixture.service.startCheck();
    captureCheckHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");
    checkPending.complete(checkFailCaptor.getValue().apply(failure));
    clearInvocations(fixture.worker);
    now.set(START.plusSeconds(29));

    fixture.service.startCheck();

    verifyNoInteractions(fixture.worker);

    now.set(START.plusSeconds(30));

    fixture.service.startCheck();

    verify(fixture.worker).start(any(), any(), any(), any());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldCheckAgainAtTheNextCallAfterEveryOtherFailureInsideTheRenewWindow(Kind kind) {
    fixture.properties.setCheckSeconds(3600);
    now.set(Instant.parse("2026-01-01T12:56:00Z"));
    fixture.service.startCheck();
    captureCheckHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");
    checkPending.complete(checkFailCaptor.getValue().apply(failure));
    clearInvocations(fixture.worker);
    now.set(Instant.parse("2026-01-01T12:56:01Z"));

    fixture.service.startCheck();

    verify(fixture.worker).start(any(), any(), any(), any());
  }

  @Test
  void shouldIgnoreFailureOfStaleCheck() {
    fixture.service.startCheck();
    captureCheckHooks();
    fixture.service.startLogout();
    clearInvocations(fixture.sessionStore);
    BBjAdminFailure failure = new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");

    checkPending.complete(checkFailCaptor.getValue().apply(failure));

    verify(fixture.sessionStore, never()).clearSession();
    assertEquals(List.of(), getInfoMessages());
  }

  @Test
  void shouldCloseLateRenewedSession() {
    fixture.service.startCheck();
    captureCheckHooks();

    fixture.discardCaptor.getValue().accept(renewed);

    verify(fixture.gateway).closeSession(renewed);
  }

  @Test
  void shouldKeepTheTokenOfLateSessionThatWasNotRenewed() {
    fixture.service.startCheck();
    captureCheckHooks();

    fixture.discardCaptor.getValue().accept(fixture.session);

    verify(fixture.gateway, never()).closeSession(any());
  }

  @Test
  void shouldLeaveRunningCheckAloneOnLogout() {
    fixture.service.startCheck();

    fixture.service.startLogout();

    assertFalse(checkPending.isCancelled());
  }

  @Test
  void shouldLeaveRunningCheckAloneOnNewSignIn() {
    fixture.service.startCheck();

    fixture.service.startLogin("alice", PASSWORD);

    assertFalse(checkPending.isCancelled());
  }

  @ParameterizedTest
  @EnumSource(Kind.class)
  void shouldKeepTheUserNameAndTheTokenOutOfEveryLogRecord(Kind kind) {
    fixture.service.startCheck();
    captureCheckHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    checkPending.complete(checkFailCaptor.getValue().apply(failure));

    SimpleFormatter formatter = new SimpleFormatter();
    assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains("alice")
        || formatter.format(it).contains("tok-abc-123")));
  }
}
