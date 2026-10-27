package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.verifyNoInteractions;

import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.EnumSource;

class BBjAuthenticationServiceFailureKindsTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldGiveInvalidCredentialsWithoutWarning() {
    fixture.whenOpenSession("alice", PASSWORD)
        .thenThrow(new BBjAdminFailure(Kind.INVALID_CREDENTIALS, "refused"));

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, status);
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "INVALID_CREDENTIALS")
  void shouldGiveUnavailableWithOneWarning(Kind kind) {
    fixture.whenOpenSession("alice", PASSWORD).thenThrow(new BBjAdminFailure(kind, "not served"));

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertEquals(1, fixture.getWarnings().size());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, names = {"TOKEN_REJECTED", "UNAVAILABLE"})
  void shouldLogTheFailureMessageWhenTheSignInCouldNotComplete(Kind kind) {
    fixture.whenOpenSession("alice", PASSWORD)
        .thenThrow(new BBjAdminFailure(kind, "NoClassDefFoundError: Factory"));

    fixture.login("alice", PASSWORD);

    assertEquals(
        List.of("The BBj Admin API could not complete the sign in: NoClassDefFoundError: Factory"),
        fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(Kind.class)
  void shouldKeepNoSessionAndNotNavigate(Kind kind) {
    fixture.whenOpenSession("alice", PASSWORD).thenThrow(new BBjAdminFailure(kind, "not served"));

    fixture.login("alice", PASSWORD);

    assertFalse(fixture.currentUser().isPresent());
    verifyNoInteractions(fixture.navigator);
  }

  @Test
  void shouldGiveUnavailableWhenTheGatewayThrowsAnyOtherRuntimeException() {
    fixture.whenOpenSession("alice", PASSWORD).thenThrow(new IllegalStateException("alice"));

    BBjLoginStatus status = fixture.login("alice", PASSWORD);

    assertEquals(BBjLoginStatus.UNAVAILABLE, status);
    assertEquals(List.of("The BBj Admin API could not complete the sign in: "
        + "The Admin API call could not be completed"), fixture.getWarnings());
    assertFalse(fixture.currentUser().isPresent());
    verifyNoInteractions(fixture.navigator);
  }
}
