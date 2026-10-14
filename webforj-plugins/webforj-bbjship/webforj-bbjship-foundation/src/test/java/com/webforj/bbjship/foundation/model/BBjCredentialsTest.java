package com.webforj.bbjship.foundation.model;

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

  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret";
  private static final String FACTORY_USERNAME = "admin";
  private static final String FACTORY_PASSWORD = "admin123";

  @Test
  void shouldHoldTheGivenValues() {
    BBjCredentials credentials = BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD);

    assertEquals(ADMIN_USERNAME, credentials.getUsername());
    assertEquals(ADMIN_PASSWORD, credentials.getPassword());
    assertFalse(credentials.isToken());
    assertNull(credentials.getToken());
  }

  @Test
  void shouldHoldTheGivenToken() {
    BBjCredentials credentials = BBjCredentials.createToken("4f2a9c");

    assertEquals("4f2a9c", credentials.getToken());
    assertTrue(credentials.isToken());
    assertNull(credentials.getUsername());
    assertNull(credentials.getPassword());
  }

  @Test
  void shouldRejectNullValues() {
    String username = ADMIN_USERNAME;
    String password = ADMIN_PASSWORD;
    String missing = null;

    assertThrows(NullPointerException.class, () -> BBjCredentials.createUser(missing, password));
    assertThrows(NullPointerException.class, () -> BBjCredentials.createUser(username, missing));
    assertThrows(NullPointerException.class, () -> BBjCredentials.createToken(missing));
  }

  @Test
  void shouldHoldTheFactoryAccount() {
    BBjCredentials credentials = BBjCredentials.createFactoryAccount();

    assertEquals(FACTORY_USERNAME, credentials.getUsername());
    assertEquals(FACTORY_PASSWORD, credentials.getPassword());
    assertTrue(credentials.isFactoryAccount());
  }

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldTellTheFactoryAccountFromOtherCredentials(BBjCredentials credentials,
      boolean expected) {
    assertEquals(expected, credentials.isFactoryAccount());
  }

  private static Stream<Arguments> getCredentials() {
    return Stream.of(
        Arguments.of(BBjCredentials.createUser(FACTORY_USERNAME, FACTORY_PASSWORD), true),
        Arguments.of(BBjCredentials.createUser(FACTORY_USERNAME, ADMIN_PASSWORD), false),
        Arguments.of(BBjCredentials.createUser(ADMIN_USERNAME, FACTORY_PASSWORD), false),
        Arguments.of(BBjCredentials.createToken(FACTORY_PASSWORD), false));
  }
}
