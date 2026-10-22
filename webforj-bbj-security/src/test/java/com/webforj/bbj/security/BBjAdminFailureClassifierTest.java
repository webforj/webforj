package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminSecurityException;
import com.webforj.bbj.security.BBjAdminFailure.Kind;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjAdminFailureClassifierTest {
  private static final String PASSWORD = "pw-secret-1";
  private static final String TOKEN = "tokenvalue1234";

  private static Stream<Arguments> getKnownTexts() {
    return Stream.of(
        Arguments.of("Unable to authenticate user. Invalid username/password.",
            Kind.INVALID_CREDENTIALS),
        Arguments.of("Unable to authenticate user. Authentication failed. "
            + "Invalid 2-factor authentication code.", Kind.INVALID_CREDENTIALS),
        Arguments.of("Unable to authenticate user. Authentication failed. "
            + "2-factor authentication code required.", Kind.TWO_FACTOR_REQUIRED),
        Arguments.of("Server failed login: Authentication token is expired", Kind.TOKEN_REJECTED),
        Arguments.of("Server failed login: Unable to verify authentication token",
            Kind.TOKEN_REJECTED),
        Arguments.of(
            "Server failed login: Unable to verify authentication token " + "(invalidated token)",
            Kind.TOKEN_REJECTED),
        Arguments.of("Server failed login: some reason nobody knows", Kind.TOKEN_REJECTED),
        Arguments.of("Unable to communicate with server.  Server may have gone down.",
            Kind.UNAVAILABLE),
        Arguments.of("BBjAdmin is released", Kind.UNAVAILABLE),
        Arguments.of("Server did not accept the connection", Kind.UNAVAILABLE),
        Arguments
            .of("Server protocol version 5 is not compatible with the client", Kind.UNAVAILABLE),
        Arguments.of("Unable to create SSL socket", Kind.UNAVAILABLE),
        Arguments.of(
            "(certificate_unknown) None of the TrustManagers trust this certificate " + "chain",
            Kind.UNAVAILABLE),
        Arguments.of("java.net.ConnectException: Connection refused", Kind.UNAVAILABLE),
        Arguments.of("java.net.SocketTimeoutException: connect timed out", Kind.UNAVAILABLE),
        Arguments.of("java.net.SocketTimeoutException: Read timed out", Kind.UNAVAILABLE),
        Arguments.of("something the library has never said", Kind.UNAVAILABLE),
        Arguments.of("", Kind.UNAVAILABLE));
  }

  @ParameterizedTest
  @MethodSource("getKnownTexts")
  void shouldClassifyTheTextOfTheException(String text, Kind kind) {
    assertEquals(kind, BBjAdminFailureClassifier.toFailure(new BBjAdminException(text)).getKind());
  }

  @ParameterizedTest
  @MethodSource("getKnownTexts")
  void shouldClassifyTheTextOfACause(String text, Kind kind) {
    BBjAdminException wrapped = new BBjAdminException(new IOException(text));

    assertEquals(kind, BBjAdminFailureClassifier.toFailure(wrapped).getKind());
  }

  @Test
  void shouldClassifyANullMessageAsUnavailable() {
    BBjAdminException exception = new BBjAdminException((String) null);

    assertEquals(Kind.UNAVAILABLE, BBjAdminFailureClassifier.toFailure(exception).getKind());
  }

  @Test
  void shouldClassifyTheTextWhateverItsCase() {
    BBjAdminException exception =
        new BBjAdminException("UNABLE TO AUTHENTICATE USER. INVALID USERNAME/PASSWORD.");

    assertEquals(Kind.INVALID_CREDENTIALS,
        BBjAdminFailureClassifier.toFailure(exception).getKind());
  }

  @Test
  void shouldPreferTheRefusedLoginPrefixToAKnownText() {
    BBjAdminException exception = new BBjAdminException(
        "Server failed login: Authentication failed for user '***'. Invalid username/password.");

    assertEquals(Kind.TOKEN_REJECTED, BBjAdminFailureClassifier.toFailure(exception).getKind());
  }

  @Test
  void shouldClassifyAPermissionRefusalAsUnavailable() {
    BBjAdminSecurityException exception = new BBjAdminSecurityException(
        "User 'x' does not have permission to access server configurations.");

    assertEquals(Kind.UNAVAILABLE, BBjAdminFailureClassifier.toFailure(exception).getKind());
  }

  @Test
  void shouldNeverTakeAVerdictFromAnExceptionThatIsNotAnAdminException() {
    IllegalStateException exception = new IllegalStateException("Invalid username/password");
    UnknownHostException unknownHost = new UnknownHostException("Invalid username/password");

    assertEquals(Kind.UNAVAILABLE, BBjAdminFailureClassifier.toFailure(exception).getKind());
    assertEquals(Kind.UNAVAILABLE, BBjAdminFailureClassifier.toFailure(unknownHost).getKind());
  }

  @Test
  void shouldLookOnlyFiveCausesDeep() {
    Throwable chain = new BBjAdminException("Invalid username/password");
    for (int index = 0; index < 5; index++) {
      chain = new BBjAdminException("level " + index, chain);
    }

    assertEquals(Kind.UNAVAILABLE, BBjAdminFailureClassifier.toFailure(chain).getKind());
  }

  @Test
  void shouldStopAtACycleOfCauses() {
    BBjAdminException first = new BBjAdminException("first");
    BBjAdminException second = new BBjAdminException("second", first);
    first.initCause(second);

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(first);

    assertEquals(
        "BBj Admin API is unavailable: BBjAdminException: first" + " / BBjAdminException: second",
        failure.getMessage());
  }

  @Test
  void shouldRefuseANullCause() {
    assertThrows(NullPointerException.class, () -> BBjAdminFailureClassifier.toFailure(null));
  }

  @Test
  void shouldDescribeTheCauseChainWithoutDuplicates() {
    BBjAdminException exception = new BBjAdminException("boom", new BBjAdminException("boom"));

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception);

    assertEquals("BBj Admin API is unavailable: BBjAdminException: boom", failure.getMessage());
  }

  @Test
  void shouldNameAnExceptionWithoutAMessage() {
    BBjAdminFailure failure =
        BBjAdminFailureClassifier.toFailure(new BBjAdminException(new IllegalStateException()));

    assertEquals("BBj Admin API is unavailable: BBjAdminException: java.lang.IllegalStateException"
        + " / IllegalStateException", failure.getMessage());
  }

  @Test
  void shouldNameAnAnonymousExceptionByItsClassName() {
    BBjAdminException anonymous = new BBjAdminException("x") {};

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(anonymous);

    assertTrue(failure.getMessage().startsWith(
        "BBj Admin API is unavailable: " + BBjAdminFailureClassifierTest.class.getName()));
  }

  @Test
  void shouldHaveNoCauseAndNoSuppressedExceptions() {
    BBjAdminException exception = new BBjAdminException("boom");
    exception.addSuppressed(new IllegalStateException("suppressed"));

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception);

    assertNull(failure.getCause());
    assertEquals(0, failure.getSuppressed().length);
  }

  @Test
  void shouldReplaceTheSecretsInTheMessage() {
    BBjAdminException exception =
        new BBjAdminException("bad " + PASSWORD + " and " + TOKEN + " again " + PASSWORD);

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception, PASSWORD, TOKEN);

    assertEquals("BBj Admin API is unavailable: BBjAdminException: bad *** and *** again ***",
        failure.getMessage());
    assertFalse(failure.toString().contains(PASSWORD));
    assertFalse(failure.toString().contains(TOKEN));
  }

  @Test
  void shouldReplaceTheLongestSecretFirst() {
    BBjAdminException exception = new BBjAdminException("abcdef");

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception, "abc", "abcdef");

    assertEquals("BBj Admin API is unavailable: BBjAdminException: ***", failure.getMessage());
  }

  @Test
  void shouldIgnoreEmptyAndNullSecrets() {
    BBjAdminException exception = new BBjAdminException("boom");

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception, "", null, PASSWORD);
    BBjAdminFailure withoutSecrets =
        BBjAdminFailureClassifier.toFailure(exception, (String[]) null);

    assertEquals("BBj Admin API is unavailable: BBjAdminException: boom", failure.getMessage());
    assertEquals(failure.getMessage(), withoutSecrets.getMessage());
  }

  @Test
  void shouldReplaceATokenThatWasNotHandedIn() {
    BBjAdminException exception =
        new BBjAdminException("got eyJhbGciOiJIUzI1NiJ9.eyJ1c2VyIjoiYSJ9.c2ln_-x here");

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception);

    assertEquals("BBj Admin API is unavailable: BBjAdminException: got *** here",
        failure.getMessage());
  }

  @Test
  void shouldReplaceControlCharacters() {
    BBjAdminException exception = new BBjAdminException("one\ntwo\r\tthree\u007ffour");

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception);

    assertEquals("BBj Admin API is unavailable: BBjAdminException: one two  three four",
        failure.getMessage());
  }

  @Test
  void shouldCutTheDetailAtThreeHundredCharacters() {
    BBjAdminException exception = new BBjAdminException("x".repeat(1000));

    BBjAdminFailure failure = BBjAdminFailureClassifier.toFailure(exception);

    assertEquals("BBj Admin API is unavailable: ".length() + 300, failure.getMessage().length());
  }

  @Test
  void shouldNameTheKindInTheMessage() {
    assertEquals(
        "BBj Admin API refused the credentials: BBjAdminException: Invalid username/password",
        BBjAdminFailureClassifier.toFailure(new BBjAdminException("Invalid username/password"))
            .getMessage());
    assertEquals(
        "BBj Admin API requires a 2-factor code: BBjAdminException: 2-factor authentication code required",
        BBjAdminFailureClassifier
            .toFailure(new BBjAdminException("2-factor authentication code required"))
            .getMessage());
    assertEquals(
        "BBj Admin API refused the token: BBjAdminException: Server failed login: Authentication token is expired",
        BBjAdminFailureClassifier
            .toFailure(
                new BBjAdminException("Server failed login: Authentication token is expired"))
            .getMessage());
  }
}
