package com.webforj.bbj.security.authenticationservice;

import static org.junit.jupiter.api.Assertions.assertFalse;

import com.webforj.bbj.security.SignInFixture;
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
    assertFalse(fixture.currentUser().isPresent());
  }
}
