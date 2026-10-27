package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAuthenticationServiceLoginInputTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldRefuseNullUserName() {
    assertThrows(NullPointerException.class, () -> fixture.login(null, PASSWORD));
  }

  @Test
  void shouldRefuseNullPassword() {
    assertThrows(NullPointerException.class, () -> fixture.login("alice", null));
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getRefusedInputs")
  void shouldGiveInvalidCredentialsWithoutCallingAnyone(String userName, String password) {
    BBjLoginStatus status = fixture.login(userName, password);

    assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, status);
    verifyNoInteractions(fixture.gateway);
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getAcceptedInputs")
  void shouldAcceptInputAtTheLimit(String userName, String password) {
    fixture.login(userName, password);

    fixture.verifyOpenSession(times(1), userName, password);
  }
}
