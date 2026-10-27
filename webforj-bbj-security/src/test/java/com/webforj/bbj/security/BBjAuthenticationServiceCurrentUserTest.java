package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.verifyNoInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

class BBjAuthenticationServiceCurrentUserTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldBeEmptyBeforeAnySignIn() {
    assertFalse(fixture.service.currentUser().isPresent());
  }

  @Test
  void shouldNeverKeepDiscardedSession() {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();

    fixture.discardCaptor.getValue().accept(fixture.session);

    assertFalse(fixture.service.currentUser().isPresent());
    verifyNoInteractions(fixture.navigator);
  }
}
