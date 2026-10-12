package com.webforj.plugin.foundation.bbj.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjCredentialsTest {

  @Test
  void shouldHoldTheGivenValues() {
    BBjCredentials credentials = BBjCredentials.of("administrator", "secret");

    assertEquals("administrator", credentials.getUsername());
    assertEquals("secret", credentials.getPassword());
    assertFalse(credentials.isToken());
    assertNull(credentials.getToken());
  }

  @Test
  void shouldHoldTheGivenToken() {
    BBjCredentials credentials = BBjCredentials.ofToken("4f2a9c");

    assertEquals("4f2a9c", credentials.getToken());
    assertTrue(credentials.isToken());
    assertNull(credentials.getUsername());
    assertNull(credentials.getPassword());
  }

  @Test
  void shouldRejectNullValues() {
    String username = "administrator";
    String password = "secret";
    String missing = null;

    assertThrows(NullPointerException.class, () -> BBjCredentials.of(missing, password));
    assertThrows(NullPointerException.class, () -> BBjCredentials.of(username, missing));
    assertThrows(NullPointerException.class, () -> BBjCredentials.ofToken(missing));
  }

  @Test
  void shouldHoldTheFactoryAccount() {
    BBjCredentials credentials = BBjCredentials.ofFactoryAccount();

    assertEquals("admin", credentials.getUsername());
    assertEquals("admin123", credentials.getPassword());
    assertTrue(credentials.isFactoryAccount());
  }

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldTellTheFactoryAccountFromOtherCredentials(BBjCredentials credentials,
      boolean expected) {
    assertEquals(expected, credentials.isFactoryAccount());
  }

  private static Stream<Arguments> getCredentials() {
    return Stream.of(Arguments.of(BBjCredentials.of("admin", "admin123"), true),
        Arguments.of(BBjCredentials.of("admin", "secret"), false),
        Arguments.of(BBjCredentials.of("administrator", "admin123"), false),
        Arguments.of(BBjCredentials.ofToken("admin123"), false));
  }
}
