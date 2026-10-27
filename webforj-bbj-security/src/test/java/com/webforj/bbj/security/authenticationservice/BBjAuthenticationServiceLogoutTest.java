package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.router.history.Location;
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
    clearInvocations(fixture.navigator, fixture.locationStore, fixture.gateway);
  }

  @Test
  void shouldCallNoOneWhenNobodySignedIn() {
    fixture.logout();

    verifyNoInteractions(fixture.navigator, fixture.gateway, fixture.locationStore);
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

    verify(fixture.sessionStore).clearSession();
  }

  @Test
  void shouldClearTheLocationStore() {
    startSignedInSession();

    fixture.logout();

    verify(fixture.locationStore).clearLocation();
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
  void shouldActLikeNobodySignedInOnSecondLogout() {
    startSignedInSession();
    fixture.logout();
    clearInvocations(fixture.navigator, fixture.locationStore, fixture.gateway);

    fixture.logout();

    verifyNoInteractions(fixture.navigator, fixture.gateway, fixture.locationStore);
  }
}
