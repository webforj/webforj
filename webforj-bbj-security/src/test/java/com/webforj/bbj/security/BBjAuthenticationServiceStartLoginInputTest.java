package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static com.webforj.bbj.security.SignInFixture.getStatus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.PendingResult;
import com.webforj.bbj.security.model.BBjLoginStatus;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAuthenticationServiceStartLoginInputTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldRefuseNullUserName() {
    assertThrows(NullPointerException.class, () -> fixture.service.startLogin(null, PASSWORD));
  }

  @Test
  void shouldRefuseNullPassword() {
    assertThrows(NullPointerException.class, () -> fixture.service.startLogin("alice", null));
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getRefusedInputs")
  void shouldCompleteWithInvalidCredentialsWithoutCallingAnyone(String userName, String password)
      throws Exception {
    PendingResult<BBjLoginStatus> result = fixture.service.startLogin(userName, password);

    assertTrue(result.isDone());
    assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, getStatus(result));
    verifyNoInteractions(fixture.worker, fixture.gateway);
  }

  @ParameterizedTest
  @MethodSource("com.webforj.bbj.security.SignInFixture#getAcceptedInputs")
  void shouldAcceptInputAtTheLimit(String userName, String password) {
    fixture.service.startLogin(userName, password);

    fixture.captureHooks();
  }
}
