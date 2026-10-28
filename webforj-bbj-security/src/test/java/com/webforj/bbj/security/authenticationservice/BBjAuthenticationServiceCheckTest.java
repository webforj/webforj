package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjPrincipal;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.bbj.security.store.BBjSecurityStore;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
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

class BBjAuthenticationServiceCheckTest {
  private static final Instant START = Instant.parse("2026-01-01T12:00:00Z");
  private final SignInFixture fixture = new SignInFixture();
  private final BBjPrincipal renewedPrincipal =
      new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());
  private final BBjSession renewed =
      new BBjSession(renewedPrincipal, "tok-new-456", Instant.parse("2026-01-01T14:00:00Z"));
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

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @BeforeEach
  void setUpCheck() {
    fixture.properties.setCheckSeconds(30);
    fixture.properties.setRenewBeforeMinutes(5);
    fixture.whenReadUserName(fixture.session).thenReturn("alice");
    fixture.setStoredSession(fixture.session);
    fixture.setService(fixture.createBuilder().setClock(clock).build());
    fixture.restoreSession();
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
  void shouldCallNoOneWhenNobodySignedIn() {
    BBjSecurityStore emptyStore = mock(BBjSecurityStore.class);
    when(emptyStore.readSession()).thenReturn(Optional.empty());
    fixture.setService(fixture.createBuilder().setStore(emptyStore).setClock(clock).build());
    fixture.restoreSession();

    fixture.check();

    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldDropTheSignInWithNoServerCallWhenAnotherTabSignedOut() {
    fixture.setStoredSession(null);

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store, never()).clearSession();
    verifyNoInteractions(fixture.gateway, fixture.navigator);
    assertEquals(List.of("The session store holds no sign in, the sign in was dropped"),
        fixture.getDebugMessages());
  }

  @Test
  void shouldTakeTheSignInOfAnotherTabWhenThisTabHoldsNone() {
    fixture.setService(fixture.createBuilder().setClock(clock).build());

    fixture.check();

    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @Test
  void shouldTakeAndCheckTheSessionAnotherTabRenewed() {
    fixture.check();
    clearInvocations(fixture.gateway);
    fixture.setStoredSession(renewed);
    fixture.whenReadUserName(renewed).thenReturn("alice");
    now.set(START.plusSeconds(1));

    fixture.check();

    assertSame(renewedPrincipal, fixture.currentUser().orElseThrow());
    fixture.verifyReadUserName(times(1), renewed);
    verify(fixture.store, never()).saveSession(any());
  }

  @Test
  void shouldTakeTheSessionAnotherTabRenewedWhenTheCheckedTokenWasRejected() {
    fixture.whenReadUserName(fixture.session).thenAnswer(call -> {
      fixture.setStoredSession(renewed);
      throw new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");
    });

    fixture.check();

    assertSame(renewedPrincipal, fixture.currentUser().orElseThrow());
    verify(fixture.store, never()).clearSession();
    assertEquals(List.of(), getInfoMessages());
    assertEquals(List.of("Another sign in in the session store replaced the rejected one"),
        fixture.getDebugMessages());
  }

  @Test
  void shouldSignOutWhenTheTokenWasRejectedAndAnotherTabSignedOutMeanwhile() {
    fixture.whenReadUserName(fixture.session).thenAnswer(call -> {
      fixture.setStoredSession(null);
      throw new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended");
    });

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store).clearSession();
    assertEquals(List.of("The server no longer accepts the sign in, the user was signed out"),
        getInfoMessages());
  }

  @ParameterizedTest
  @ValueSource(strings = {"2026-01-01T13:00:00Z", "2026-01-01T13:00:01Z"})
  void shouldClearAnExpiredSessionWithNoServerCall(String time) {
    now.set(Instant.parse(time));

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store).clearSession();
    verifyNoInteractions(fixture.gateway);
    assertEquals(List.of("The sign in expired and was cleared"), fixture.getDebugMessages());
  }

  @Test
  void shouldCheckWhenNothingWasCheckedSinceTheRestore() {
    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @Test
  void shouldAskForTheUserAndKeepTheSessionOutsideTheRenewWindow() {
    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
    fixture.verifyRenewSessionOfAny(never());
    verify(fixture.store, never()).saveSession(any());
    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldSignOutWhenTheServerKnowsAnotherUser() {
    fixture.whenReadUserName(fixture.session).thenReturn("bob");

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store).clearSession();
    fixture.verifyRenewSessionOfAny(never());
  }

  @Test
  void shouldRenewInsideTheRenewWindow() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.whenRenewSession(fixture.session).thenReturn(renewed);

    fixture.check();

    fixture.verifyRenewSession(times(1), fixture.session);
    assertSame(renewedPrincipal, fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldNeverRenewWhenRenewBeforeMinutesIsZero() {
    fixture.properties.setRenewBeforeMinutes(0);
    now.set(Instant.parse("2026-01-01T12:59:59Z"));

    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
    fixture.verifyRenewSessionOfAny(never());
  }

  @Test
  void shouldCallNoOneWhenCheckedWithinCheckSeconds() {
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(START.plusSeconds(29));

    fixture.check();

    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldCheckWhenCheckSecondsPassed() {
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(START.plusSeconds(30));

    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @Test
  void shouldCheckAtEveryCallWhenCheckSecondsIsZero() {
    fixture.properties.setCheckSeconds(0);
    fixture.check();
    clearInvocations(fixture.gateway);

    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @ParameterizedTest
  @MethodSource("getWindowCases")
  void shouldCheckInsideTheRenewWindowWhateverCheckSecondsSay(String time, boolean due) {
    fixture.properties.setCheckSeconds(3600);
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(Instant.parse(time));

    fixture.check();

    fixture.verifyReadUserName(due ? times(1) : never(), fixture.session);
  }

  @Test
  void shouldTreatTheSignInAsTheLastCheck() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
    fixture.login("alice", PASSWORD);
    clearInvocations(fixture.gateway);
    now.set(START.plusSeconds(29));

    fixture.check();

    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldKeepTheSessionAndLogDebugWhenTheCheckFoundNoChange() {
    fixture.check();

    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    verify(fixture.store, never()).saveSession(any());
    assertEquals(List.of("The sign in was checked and stays"), fixture.getDebugMessages());
  }

  @Test
  void shouldSaveAndKeepTheRenewedSession() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.whenRenewSession(fixture.session).thenReturn(renewed);

    fixture.check();

    verify(fixture.store).saveSession(renewed);
    assertSame(renewedPrincipal, fixture.currentUser().orElseThrow());
    assertEquals(List.of("The sign in was renewed"), getInfoMessages());
  }

  @Test
  void shouldTreatTheRenewalAsTheLastCheck() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.properties.setCheckSeconds(3600);
    fixture.whenRenewSession(fixture.session).thenReturn(renewed);
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(Instant.parse("2026-01-01T12:56:00Z"));

    fixture.check();

    verifyNoInteractions(fixture.gateway);
  }

  @Test
  void shouldSignOutAndCloseTheRenewedTokenWhenItCannotBeSaved() {
    now.set(Instant.parse("2026-01-01T12:55:00Z"));
    fixture.whenRenewSession(fixture.session).thenReturn(renewed);
    doThrow(new IllegalStateException("not kept")).when(fixture.store).saveSession(renewed);

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store).clearSession();
    fixture.verifyCloseSession(times(1), renewed);
    assertEquals(List.of("The renewed sign in could not be kept, the user was signed out"),
        fixture.getWarnings());
  }

  @Test
  void shouldSignOutAndLogInfoWhenTheTokenWasRejected() {
    fixture.setReadUserNameFailure(fixture.session,
        new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended"));

    fixture.check();

    assertFalse(fixture.currentUser().isPresent());
    verify(fixture.store).clearSession();
    assertEquals(List.of("The server no longer accepts the sign in, the user was signed out"),
        getInfoMessages());
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldKeepTheSignInAndWarnForEveryOtherFailure(Kind kind) {
    fixture.setReadUserNameFailure(fixture.session, new BBjAdminFailure(kind, "not served"));

    fixture.check();

    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    verify(fixture.store, never()).clearSession();
    assertEquals(
        List.of("The sign in could not be checked and stays until it expires: " + "not served"),
        fixture.getWarnings());
  }

  @Test
  void shouldKeepTheSignInWhenTheGatewayThrowsAnyOtherRuntimeException() {
    fixture.setReadUserNameFailure(fixture.session, new IllegalStateException("alice"));

    fixture.check();

    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    verify(fixture.store, never()).clearSession();
    assertEquals(List.of("The sign in could not be checked and stays until it expires: "
        + "The Admin API call could not be completed"), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldWaitCheckSecondsAfterEveryOtherFailureOutsideTheRenewWindow(Kind kind) {
    fixture.setReadUserNameFailure(fixture.session, new BBjAdminFailure(kind, "not served"));
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(START.plusSeconds(29));

    fixture.check();

    verifyNoInteractions(fixture.gateway);

    now.set(START.plusSeconds(30));

    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldCheckAgainAtTheNextCallAfterEveryOtherFailureInsideTheRenewWindow(Kind kind) {
    fixture.properties.setCheckSeconds(3600);
    now.set(Instant.parse("2026-01-01T12:56:00Z"));
    fixture.setReadUserNameFailure(fixture.session, new BBjAdminFailure(kind, "not served"));
    fixture.check();
    clearInvocations(fixture.gateway);
    now.set(Instant.parse("2026-01-01T12:56:01Z"));

    fixture.check();

    fixture.verifyReadUserName(times(1), fixture.session);
  }

  @ParameterizedTest
  @EnumSource(Kind.class)
  void shouldKeepTheUserNameAndTheTokenOutOfEveryLogRecord(Kind kind) {
    fixture.setReadUserNameFailure(fixture.session, new BBjAdminFailure(kind, "not served"));

    fixture.check();

    SimpleFormatter formatter = new SimpleFormatter();
    assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains("alice")
        || formatter.format(it).contains("tok-abc-123")));
  }
}
