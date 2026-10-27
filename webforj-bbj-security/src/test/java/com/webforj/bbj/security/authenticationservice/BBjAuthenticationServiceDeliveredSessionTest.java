package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAuthenticationServiceDeliveredSessionTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldAuthenticateAndExposeThePrincipal() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.AUTHENTICATED, status);
    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldCloseTheTokenOfTheSessionItReplaces() {
    BBjSession replacing =
        new BBjSession(fixture.principal, "tok-def-456", Instant.parse("2026-01-01T14:00:00Z"));
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session).thenReturn(replacing);
    fixture.login("alice", PASSWORD);
    clearInvocations(fixture.gateway);

    fixture.login("alice", PASSWORD);

    fixture.verifyOpenSession(times(1), "alice", PASSWORD);
    fixture.verifyCloseSession(times(1), fixture.session);
    verifyNoMoreInteractions(fixture.gateway);
    assertSame(replacing.getPrincipal(), fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldSaveTheSessionInTheStore() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);

    fixture.login("alice", PASSWORD);

    verify(fixture.sessionStore).saveSession(fixture.session);
  }

  @Test
  void shouldGiveUnavailableAndKeepNobodyWhenTheSaveFails() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
    doThrow(new IllegalStateException("not kept")).when(fixture.sessionStore)
        .saveSession(fixture.session);

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertFalse(fixture.currentUser().isPresent());
    fixture.verifyCloseSession(times(1), fixture.session);
  }

  @Test
  void shouldKeepThePreviousSessionAndCloseOnlyTheNewTokenWhenTheSaveFails() {
    BBjSession replacing =
        new BBjSession(fixture.principal, "tok-def-456", Instant.parse("2026-01-01T14:00:00Z"));
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session).thenReturn(replacing);
    fixture.login("alice", PASSWORD);
    clearInvocations(fixture.gateway);
    doThrow(new IllegalStateException("not kept")).when(fixture.sessionStore)
        .saveSession(replacing);

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
    fixture.verifyOpenSession(times(1), "alice", PASSWORD);
    fixture.verifyCloseSession(times(1), replacing);
    verifyNoMoreInteractions(fixture.gateway);
  }

  @Test
  void shouldNotNavigateOnLogin() {
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);

    fixture.login("alice", PASSWORD);

    verifyNoInteractions(fixture.navigator);
    verify(fixture.locationStore, never()).clearLocation();
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getNavigationCases")
  void shouldReadClearAndNavigate(boolean returnAfterLogin, Optional<Location> saved,
      Location expected) {
    fixture.properties.setReturnAfterLogin(returnAfterLogin);
    when(fixture.locationStore.readLocation()).thenReturn(saved);
    fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
    fixture.login("alice", PASSWORD);

    fixture.openTargetLocation();

    verify(fixture.locationStore).readLocation();
    verify(fixture.locationStore).clearLocation();
    verify(fixture.navigator).accept(expected);
  }
}
