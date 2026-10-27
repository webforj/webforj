package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class BBjAuthenticationServiceWorkerCallTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldHandTheWorkerFourHooks() {
    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);

    fixture.captureHooks();
    assertSame(fixture.pending, result);
    assertNotNull(fixture.workCaptor.getValue());
    assertNotNull(fixture.deliverCaptor.getValue());
    assertNotNull(fixture.failCaptor.getValue());
    assertNotNull(fixture.discardCaptor.getValue());
  }

  @Test
  void shouldCancelTheRunningSignInWhenSecondOneStarts() {
    PendingResult<BBjLoginStatus> second = new PendingResult<>();
    doReturn(fixture.pending).doReturn(second).when(fixture.worker).start(any(), any(), any(),
        any());
    fixture.service.startLogin("alice", PASSWORD);

    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);

    assertTrue(fixture.pending.isCancelled());
    assertSame(second, result);
    assertFalse(second.isCancelled());
  }

  @Test
  void shouldOpenTheSessionInTheWork() {
    when(fixture.gateway.openSession("alice", PASSWORD)).thenReturn(fixture.session);
    fixture.service.startLogin("alice", PASSWORD);

    fixture.captureHooks();

    assertSame(fixture.session, fixture.workCaptor.getValue().get());
  }
}
