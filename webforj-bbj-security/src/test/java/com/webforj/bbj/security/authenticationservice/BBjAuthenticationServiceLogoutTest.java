package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAuthenticationServiceLogoutTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  private void startSignedInSession() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
    fixture.login("alice", PASSWORD);
    clearInvocations(fixture.navigator, fixture.store, fixture.gateway);
  }

  @Test
  void shouldCallNoOneWhenNobodySignedIn() {
    fixture.logout();

    verifyNoInteractions(fixture.navigator, fixture.gateway);
    verify(fixture.store, never()).clearSession();
  }

  @Test
  void shouldForgetTheUserRightAway() {
    startSignedInSession();

    fixture.logout();

    assertFalse(fixture.currentUser().isPresent());
  }

  @Test
  void shouldClearTheSessionStore() {
    startSignedInSession();

    fixture.logout();

    verify(fixture.store).clearSession();
  }

  @Test
  void shouldClearTheRequestedLocation() {
    startSignedInSession();

    fixture.logout();

    verify(fixture.securityManager).clearPreAuthenticationLocation();
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getLogoutCases")
  void shouldOpenTheLogoutLocationOfTheConfiguration(String logoutPath, Location expected) {
    fixture.properties.setLogoutSuccessPath(logoutPath);
    startSignedInSession();

    fixture.logout();

    verify(fixture.navigator).accept(expected);
  }

  @Test
  void shouldCloseTheSignedInSession() {
    startSignedInSession();

    fixture.logout();

    fixture.verifyCloseSession(times(1), fixture.session);
  }

  @Test
  void shouldLogDebugWhenTheTokenWasRejected() {
    startSignedInSession();
    fixture.setCloseSessionFailure(fixture.session,
        new BBjAdminFailure(Kind.TOKEN_REJECTED, "ended"));

    fixture.logout();

    assertEquals(List.of("The token was already ended: ended"), fixture.getDebugMessages());
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "TOKEN_REJECTED")
  void shouldWarnWhenTheTokenCouldNotBeEnded(Kind kind) {
    startSignedInSession();
    fixture.setCloseSessionFailure(fixture.session, new BBjAdminFailure(kind, "not served"));

    fixture.logout();

    assertEquals(List.of("The token could not be ended and expires on its own: not served"),
        fixture.getWarnings());
    assertEquals(List.of(), fixture.getDebugMessages());
  }

  @Test
  void shouldWarnAndSignOutWhenTheGatewayThrowsAnyOtherRuntimeException() {
    startSignedInSession();
    fixture.setCloseSessionFailure(fixture.session, new IllegalStateException("alice"));

    fixture.logout();

    assertFalse(fixture.currentUser().isPresent());
    assertEquals(List.of("The token could not be ended and expires on its own: "
        + "The Admin API call could not be completed"), fixture.getWarnings());
  }

  @Test
  void shouldSignOutAndEndTheTokenWhenTheStoreIsUnavailable() {
    fixture.setStoreAvailable(false);
    startSignedInSession();

    fixture.logout();

    assertFalse(fixture.currentUser().isPresent());
    fixture.verifyCloseSession(times(1), fixture.session);
  }

  @Test
  void shouldActLikeNobodySignedInOnSecondLogout() {
    startSignedInSession();
    fixture.logout();
    clearInvocations(fixture.navigator, fixture.store, fixture.gateway);

    fixture.logout();

    verifyNoInteractions(fixture.navigator, fixture.gateway);
    verify(fixture.store, never()).clearSession();
  }

  @Test
  void shouldSignOutAStoredSessionThisServiceDoesNotHold() {
    fixture.setStoredSession(fixture.session);

    fixture.logout();

    verify(fixture.store).clearSession();
    verify(fixture.securityManager).clearPreAuthenticationLocation();
    verify(fixture.navigator).accept(new Location("/login"));
    fixture.verifyCloseSession(times(1), fixture.session);
  }

  @Test
  void shouldCloseTheTokensOfTheHeldAndTheStoredSessionWhenTheyDiffer() {
    BBjSession other =
        new BBjSession(fixture.principal, "tok-other-789", Instant.parse("2026-01-01T14:00:00Z"));
    startSignedInSession();
    fixture.setStoredSession(other);

    fixture.logout();

    fixture.verifyCloseSession(times(1), fixture.session);
    fixture.verifyCloseSession(times(1), other);
  }
}
