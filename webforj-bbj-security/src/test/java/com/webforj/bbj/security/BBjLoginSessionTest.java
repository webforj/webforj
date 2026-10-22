package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Instant;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjLoginSessionTest {
  private static final Instant EXPIRES_AT = Instant.parse("2026-01-01T00:30:00Z");
  private static final String TOKEN = "tokenvalue1234";

  @Test
  void shouldExposeTheGivenValues() {
    BBjLoginSession session = new BBjLoginSession("alice", TOKEN, EXPIRES_AT);

    assertEquals("alice", session.getUserName());
    assertEquals(TOKEN, session.getToken());
    assertEquals(EXPIRES_AT, session.getExpiresAt());
  }

  @Test
  void shouldKeepTheTokenOutOfTheDescription() {
    BBjLoginSession session = new BBjLoginSession("alice", TOKEN, EXPIRES_AT);

    assertEquals("BBjLoginSession{userName=alice, expiresAt=2026-01-01T00:30:00Z}",
        session.toString());
    assertFalse(session.toString().contains(TOKEN.substring(0, 3)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "  ", "\t"})
  void shouldRefuseABlankUserName(String userName) {
    assertThrows(IllegalArgumentException.class,
        () -> new BBjLoginSession(userName, TOKEN, EXPIRES_AT));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "  ", "\t"})
  void shouldRefuseABlankToken(String token) {
    assertThrows(IllegalArgumentException.class,
        () -> new BBjLoginSession("alice", token, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullUserName() {
    assertThrows(NullPointerException.class, () -> new BBjLoginSession(null, TOKEN, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullToken() {
    assertThrows(NullPointerException.class, () -> new BBjLoginSession("alice", null, EXPIRES_AT));
  }

  @Test
  void shouldRefuseANullExpiry() {
    assertThrows(NullPointerException.class, () -> new BBjLoginSession("alice", TOKEN, null));
  }
}
