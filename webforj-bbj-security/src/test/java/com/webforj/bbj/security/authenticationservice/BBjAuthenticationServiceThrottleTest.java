package com.webforj.bbj.security.authenticationservice;

import static com.webforj.bbj.security.SignInFixture.ADDRESS;
import static com.webforj.bbj.security.SignInFixture.PASSWORD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.webforj.bbj.security.BBjAdminFailure;
import com.webforj.bbj.security.BBjAuthenticationService;
import com.webforj.bbj.security.SignInFixture;
import com.webforj.bbj.security.model.BBjLoginStatus;
import com.webforj.bbj.security.store.BBjThrottleStore.Kind;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.EnumSource.Mode;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class BBjAuthenticationServiceThrottleTest {
  private final SignInFixture fixture = new SignInFixture();

  @AfterEach
  void tearDown() {
    fixture.close();
  }

  @Nested
  class Refusal {

    @Test
    void shouldRefuseUserNameAtItsLimitWithoutCallingBBj() {
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice")).thenReturn(5);

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.THROTTLED, status);
      verifyNoInteractions(fixture.gateway);
      verify(fixture.throttleStore, never()).addFailure(any(), anyString());
      assertEquals(List.of("A sign in was refused after too many failed attempts"),
          fixture.getDebugMessages());
    }

    @Test
    void shouldLetUserNameBelowItsLimitThrough() {
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice")).thenReturn(4);
      when(fixture.throttleStore.readAttempts(Kind.ADDRESS, ADDRESS)).thenReturn(19);
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
    }

    @Test
    void shouldRefuseAddressAtItsLimitWithoutCallingBBj() {
      when(fixture.throttleStore.readAttempts(Kind.ADDRESS, ADDRESS)).thenReturn(20);

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.THROTTLED, status);
      verifyNoInteractions(fixture.gateway);
    }

    @Test
    void shouldReadTheAddressOnlyBelowTheUserNameLimit() {
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice")).thenReturn(5);

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore, never()).readAttempts(Kind.ADDRESS, ADDRESS);
    }

    @Test
    void shouldApplyTheConfiguredLimits() {
      fixture.properties.setThrottleMaxAttempts(3);
      fixture.properties.setThrottleAddressMaxAttempts(6);
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice")).thenReturn(3);
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "bob")).thenReturn(2);
      when(fixture.throttleStore.readAttempts(Kind.ADDRESS, ADDRESS)).thenReturn(6);

      assertEquals(BBjLoginStatus.THROTTLED, fixture.login("alice", PASSWORD));
      assertEquals(BBjLoginStatus.THROTTLED, fixture.login("bob", PASSWORD));
    }

    @Test
    void shouldGiveUnavailableWithoutCallingBBjWhenTheCountersCannotBeRead() {
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice"))
          .thenThrow(new IllegalStateException("not usable"));

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.UNAVAILABLE, status);
      verifyNoInteractions(fixture.gateway);
      assertEquals(List.of("A sign in was refused because the failed attempts are unknown"),
          fixture.getWarnings());
    }
  }

  @Nested
  class Address {

    @Test
    void shouldNeitherReadNorCountTheAddressWhenItsLimitIsOff() {
      fixture.properties.setThrottleAddressMaxAttempts(0);
      when(fixture.throttleStore.readAttempts(Kind.ADDRESS, ADDRESS)).thenReturn(10000);
      fixture.whenOpenSession("alice", PASSWORD)
          .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, status);
      verifyNoInteractions(fixture.addressSupplier);
      verify(fixture.throttleStore, never()).readAttempts(Kind.ADDRESS, ADDRESS);
      verify(fixture.throttleStore, never()).addFailure(Kind.ADDRESS, ADDRESS);
      assertEquals(List.of(), fixture.getWarnings());
    }

    @ParameterizedTest
    @MethodSource("unreadableAddresses")
    void shouldApplyOnlyTheUserNameLimitAndWarnAtEachAttempt(String address, String warning) {
      when(fixture.addressSupplier.get()).thenReturn(address);
      fixture.whenOpenSession("alice", PASSWORD)
          .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));

      fixture.login("alice", PASSWORD);
      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore, times(2)).readAttempts(Kind.USER_NAME, "alice");
      verify(fixture.throttleStore, times(2)).addFailure(Kind.USER_NAME, "alice");
      verify(fixture.throttleStore, never()).readAttempts(eq(Kind.ADDRESS), any());
      verify(fixture.throttleStore, never()).addFailure(eq(Kind.ADDRESS), any());
      assertEquals(List.of(warning, warning), fixture.getWarnings());
    }

    private static Stream<Arguments> unreadableAddresses() {
      String blank = "The client address is blank, the address limit is not applied";
      return Stream.of(Arguments.of(null, blank), Arguments.of("  ", blank));
    }

    @Test
    void shouldWarnWithTheClassNameWhenTheAddressCannotBeRead() {
      when(fixture.addressSupplier.get()).thenThrow(new IllegalArgumentException(ADDRESS));
      when(fixture.throttleStore.readAttempts(Kind.USER_NAME, "alice")).thenReturn(5);

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.THROTTLED, status);
      assertEquals(List.of("The client address could not be read, the address limit is not "
          + "applied (java.lang.IllegalArgumentException)"), fixture.getWarnings());
    }
  }

  @Nested
  class Counting {

    @Test
    void shouldCountRefusedCredentialsForTheUserNameAndTheAddress() {
      fixture.whenOpenSession("alice", PASSWORD)
          .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore).addFailure(Kind.USER_NAME, "alice");
      verify(fixture.throttleStore).addFailure(Kind.ADDRESS, ADDRESS);
    }

    @ParameterizedTest
    @EnumSource(value = BBjAdminFailure.Kind.class, mode = Mode.EXCLUDE,
        names = "INVALID_CREDENTIALS")
    void shouldNotCountOtherFailures(BBjAdminFailure.Kind kind) {
      fixture.whenOpenSession("alice", PASSWORD).thenThrow(new BBjAdminFailure(kind, "not served"));

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore, never()).addFailure(any(), any());
    }

    @Test
    void shouldNotCountWhenTheGatewayThrowsAnyOtherRuntimeException() {
      fixture.whenOpenSession("alice", PASSWORD).thenThrow(new IllegalStateException("alice"));

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore, never()).addFailure(any(), any());
    }

    @Test
    void shouldWarnOnceEachCounterReachesItsLimit() {
      fixture.whenOpenSession("alice", PASSWORD)
          .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));
      when(fixture.throttleStore.addFailure(Kind.USER_NAME, "alice")).thenReturn(5);
      when(fixture.throttleStore.addFailure(Kind.ADDRESS, ADDRESS)).thenReturn(20);

      fixture.login("alice", PASSWORD);

      assertEquals(
          List.of("A user name reached 5 failed sign ins and is refused for 300 seconds",
              "The address 10.0.0.7 reached 20 failed sign ins and is refused for 300 seconds"),
          fixture.getWarnings());
    }

    @Test
    void shouldKeepInvalidCredentialsAndWarnWhenTheFailureCannotBeCounted() {
      fixture.whenOpenSession("alice", PASSWORD)
          .thenThrow(new BBjAdminFailure(BBjAdminFailure.Kind.INVALID_CREDENTIALS, "refused"));
      when(fixture.throttleStore.addFailure(Kind.USER_NAME, "alice"))
          .thenThrow(new IllegalStateException("not usable"));

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.INVALID_CREDENTIALS, status);
      assertEquals(List.of("A failed sign in could not be counted"), fixture.getWarnings());
    }
  }

  @Nested
  class Success {

    @Test
    void shouldClearTheUserNameCounterAndKeepTheAddressCounter() {
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore).clearAttempts(Kind.USER_NAME, "alice");
      verify(fixture.throttleStore, never()).clearAttempts(Kind.ADDRESS, ADDRESS);
    }

    @Test
    void shouldNotClearWhenTheSessionCannotBeSaved() {
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
      doThrow(new IllegalStateException("not kept")).when(fixture.store)
          .saveSession(fixture.session);

      fixture.login("alice", PASSWORD);

      verify(fixture.throttleStore, never()).clearAttempts(any(), any());
    }

    @Test
    void shouldStayAuthenticatedAndWarnWhenTheCounterCannotBeCleared() {
      fixture.whenOpenSession("alice", PASSWORD).thenReturn(fixture.session);
      doThrow(new IllegalStateException("not usable")).when(fixture.throttleStore)
          .clearAttempts(Kind.USER_NAME, "alice");

      BBjLoginStatus status = fixture.login("alice", PASSWORD);

      assertEquals(BBjLoginStatus.AUTHENTICATED, status);
      assertEquals(List.of("The failed sign ins of a user name could not be cleared"),
          fixture.getWarnings());
    }
  }

  @Nested
  class Construction {

    @ParameterizedTest
    @ValueSource(strings = {"throttleStore", "addressSupplier"})
    void shouldRefuseMissingThrottleValueNamingIt(String name) {
      BBjAuthenticationService.Builder builder = fixture.createBuilder();
      if (name.equals("throttleStore")) {
        builder.setThrottleStore(null);
      } else {
        builder.setAddressSupplier(null);
      }

      NullPointerException thrown = assertThrows(NullPointerException.class, builder::build);

      assertEquals(name, thrown.getMessage());
    }
  }
}
