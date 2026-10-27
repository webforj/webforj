package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static com.webforj.bbj.security.SignInFixture.getStatus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
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
  void shouldAuthenticateAndExposeThePrincipal() throws Exception {
    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();

    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));

    assertEquals(BBjLoginStatus.AUTHENTICATED, getStatus(result));
    assertSame(fixture.principal, fixture.service.currentUser().orElseThrow());
  }

  @Test
  void shouldCloseTheTokenOfTheSessionItReplaces() {
    PendingResult<BBjLoginStatus> second = new PendingResult<>();
    doReturn(fixture.pending).doReturn(second).doReturn(new PendingResult<Void>())
        .when(fixture.worker).start(any(), any(), any(), any());
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));
    clearInvocations(fixture.worker);
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    clearInvocations(fixture.worker);
    BBjSession replacing =
        new BBjSession(fixture.principal, "tok-def-456", Instant.parse("2026-01-01T14:00:00Z"));

    second.complete(fixture.deliverCaptor.getValue().apply(replacing));

    verify(fixture.worker).start(fixture.workCaptor.capture(), any(), any(), any());
    fixture.workCaptor.getValue().get();
    verify(fixture.gateway).closeSession(fixture.session);
    verifyNoMoreInteractions(fixture.gateway);
    assertSame(replacing.getPrincipal(), fixture.service.currentUser().orElseThrow());
  }

  @Test
  void shouldSaveTheSessionInTheStore() {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();

    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));

    verify(fixture.sessionStore).saveSession(fixture.session);
  }

  @Test
  void shouldGiveUnavailableAndKeepNobodyWhenTheSaveFails() {
    doThrow(new IllegalStateException("not kept")).when(fixture.sessionStore)
        .saveSession(fixture.session);
    doReturn(fixture.pending).doReturn(new PendingResult<Void>()).when(fixture.worker).start(any(),
        any(), any(), any());
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    clearInvocations(fixture.worker);

    BBjLoginStatus status = fixture.deliverCaptor.getValue().apply(fixture.session);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertFalse(fixture.service.currentUser().isPresent());
    verify(fixture.worker).start(fixture.workCaptor.capture(), any(), any(), any());
    fixture.workCaptor.getValue().get();
    verify(fixture.gateway).closeSession(fixture.session);
  }

  @Test
  void shouldKeepThePreviousSessionAndCloseOnlyTheNewTokenWhenTheSaveFails() {
    PendingResult<BBjLoginStatus> second = new PendingResult<>();
    doReturn(fixture.pending).doReturn(second).doReturn(new PendingResult<Void>())
        .when(fixture.worker).start(any(), any(), any(), any());
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));
    clearInvocations(fixture.worker);
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    clearInvocations(fixture.worker);
    BBjSession replacing =
        new BBjSession(fixture.principal, "tok-def-456", Instant.parse("2026-01-01T14:00:00Z"));
    doThrow(new IllegalStateException("not kept")).when(fixture.sessionStore)
        .saveSession(replacing);

    BBjLoginStatus status = fixture.deliverCaptor.getValue().apply(replacing);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertSame(fixture.principal, fixture.service.currentUser().orElseThrow());
    verify(fixture.worker).start(fixture.workCaptor.capture(), any(), any(), any());
    fixture.workCaptor.getValue().get();
    verify(fixture.gateway).closeSession(replacing);
    verifyNoMoreInteractions(fixture.gateway);
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getNavigationCases")
  void shouldReadClearAndNavigate(boolean returnAfterLogin, Optional<Location> saved,
      Location expected) {
    fixture.properties.setReturnAfterLogin(returnAfterLogin);
    when(fixture.locationStore.readLocation()).thenReturn(saved);
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();

    fixture.pending.complete(fixture.deliverCaptor.getValue().apply(fixture.session));

    verify(fixture.locationStore).readLocation();
    verify(fixture.locationStore).clearLocation();
    verify(fixture.navigator).accept(expected);
  }
}
