package com.webforj.bbj.security.authenticationservice;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.SignInFixture;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class BBjAuthenticationServiceRestoreSessionTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldPutTheStoredSessionInPlace() {
    when(fixture.sessionStore.readSession()).thenReturn(Optional.of(fixture.session));

    fixture.restoreSession();

    assertSame(fixture.principal, fixture.currentUser().orElseThrow());
  }

  @Test
  void shouldLeaveNobodySignedInWhenTheStoreIsEmpty() {
    when(fixture.sessionStore.readSession()).thenReturn(Optional.empty());

    fixture.restoreSession();

    assertFalse(fixture.currentUser().isPresent());
  }
}
