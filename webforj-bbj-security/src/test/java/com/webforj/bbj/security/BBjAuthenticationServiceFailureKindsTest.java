package com.webforj.bbj.security;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static com.webforj.bbj.security.SignInFixture.getStatus;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.PendingResult;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.model.BBjLoginStatus;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;

class BBjAuthenticationServiceFailureKindsTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Test
  void shouldGiveInvalidCredentialsWithoutWarning() throws Exception {
    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    BBjAdminFailure failure = new BBjAdminFailure(Kind.INVALID_CREDENTIALS, "refused");

    fixture.pending.complete(fixture.failCaptor.getValue().apply(failure));

    assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, getStatus(result));
    assertEquals(List.of(), fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, mode = Mode.EXCLUDE, names = "INVALID_CREDENTIALS")
  void shouldGiveUnavailableWithOneWarning(Kind kind) throws Exception {
    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    fixture.pending.complete(fixture.failCaptor.getValue().apply(failure));

    assertEquals(BBjLoginStatus.UNAVAILABLE, getStatus(result));
    assertEquals(1, fixture.getWarnings().size());
  }

  @ParameterizedTest
  @EnumSource(value = Kind.class, names = {"TOKEN_REJECTED", "UNAVAILABLE"})
  void shouldLogTheFailureMessageWhenTheSignInCouldNotComplete(Kind kind) {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "NoClassDefFoundError: Factory");

    fixture.pending.complete(fixture.failCaptor.getValue().apply(failure));

    assertEquals(
        List.of("The BBj Admin API could not complete the sign in: NoClassDefFoundError: Factory"),
        fixture.getWarnings());
  }

  @ParameterizedTest
  @EnumSource(Kind.class)
  void shouldKeepNoSessionAndNotNavigate(Kind kind) {
    fixture.service.startLogin("alice", PASSWORD);
    fixture.captureHooks();
    BBjAdminFailure failure = new BBjAdminFailure(kind, "not served");

    fixture.pending.complete(fixture.failCaptor.getValue().apply(failure));

    assertFalse(fixture.service.currentUser().isPresent());
    verifyNoInteractions(fixture.navigator);
  }

  @Test
  void shouldGiveUnavailableWhenTheWorkerRefuses() throws Exception {
    doReturn(PendingResult.completedWith(BBjLoginStatus.UNAVAILABLE)).when(fixture.worker)
        .start(any(), any(), any(), any());

    PendingResult<BBjLoginStatus> result = fixture.service.startLogin("alice", PASSWORD);

    assertEquals(BBjLoginStatus.UNAVAILABLE, getStatus(result));
    assertFalse(fixture.service.currentUser().isPresent());
  }
}
