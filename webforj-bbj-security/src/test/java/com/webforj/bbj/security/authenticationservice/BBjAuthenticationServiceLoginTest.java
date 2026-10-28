package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.model.BBjSession;
import com.webforj.router.history.Location;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import java.util.logging.SimpleFormatter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAuthenticationServiceLoginTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Nested
  class Input {

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

  @Nested
  class DeliveredSession {

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

      verify(fixture.store).saveSession(fixture.session);
    }

    @Test
    void shouldGiveUnavailableAndKeepNobodyWhenTheSaveFails() {
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
      doThrow(new IllegalStateException("not kept")).when(fixture.store)
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
      doThrow(new IllegalStateException("not kept")).when(fixture.store).saveSession(replacing);

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
      verify(fixture.store, never()).clearLocation();
    }

    @ParameterizedTest
    @MethodSource("com.webforj.bbj.security.SignInFixture#getNavigationCases")
    void shouldReadClearAndNavigate(boolean returnAfterLogin, Optional<Location> saved,
        Location expected) {
      fixture.properties.setReturnAfterLogin(returnAfterLogin);
      when(fixture.store.readLocation()).thenReturn(saved);
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
      fixture.login("alice", PASSWORD);

      fixture.openTargetLocation();

      verify(fixture.store).readLocation();
      verify(fixture.store).clearLocation();
      verify(fixture.navigator).accept(expected);
    }
  }

  @Nested
  class Failures {

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
          List.of(
              "The BBj Admin API could not complete the sign in: NoClassDefFoundError: Factory"),
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

  @Nested
  class Secrets {

    @ParameterizedTest
    @EnumSource(Kind.class)
    void shouldKeepThePasswordOutOfEveryLogRecord(Kind kind) {
      fixture.whenOpenSession("alice", PASSWORD).thenThrow(new BBjAdminFailure(kind, "not served"));

      fixture.login("alice", PASSWORD);

      SimpleFormatter formatter = new SimpleFormatter();
      assertTrue(fixture.records.stream().noneMatch(it -> formatter.format(it).contains(PASSWORD)));
    }
  }
}
