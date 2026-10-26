package com.webforj.bbj.security.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.Instant;
import java.util.Set;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class BBjSessionTest {
  private static final Instant EXPIRES_AT = Instant.parse("2026-01-01T00:30:00Z");
  private static final String TOKEN = "tokenvalue1234";
  private final BBjPrincipal principal = new BBjPrincipal("alice", Set.of("ROLE_USER"), Set.of());

  @Nested
  class Getters {

    @Test
    void shouldExposeTheGivenValues() {
      BBjSession session = new BBjSession(principal, TOKEN, EXPIRES_AT);

      assertSame(principal, session.getPrincipal());
      assertEquals(TOKEN, session.getToken());
      assertEquals(EXPIRES_AT, session.getExpiresAt());
    }
  }

  @Nested
  class Description {

    @Test
    void shouldKeepTheTokenOutOfTheDescription() {
      BBjSession session = new BBjSession(principal, TOKEN, EXPIRES_AT);

      assertEquals("BBjSession [principal=" + principal + ", expiresAt=2026-01-01T00:30:00Z]",
          session.toString());
      assertFalse(session.toString().contains(TOKEN));
    }
  }

  @Nested
  class Refusals {

    @Test
    void shouldRefuseNullPrincipal() {
      assertThrows(NullPointerException.class, () -> new BBjSession(null, TOKEN, EXPIRES_AT));
    }

    @Test
    void shouldRefuseNullToken() {
      assertThrows(NullPointerException.class, () -> new BBjSession(principal, null, EXPIRES_AT));
    }

    @Test
    void shouldRefuseNullExpiry() {
      assertThrows(NullPointerException.class, () -> new BBjSession(principal, TOKEN, null));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "  ", "\t"})
    void shouldRefuseBlankToken(String token) {
      assertThrows(IllegalArgumentException.class,
          () -> new BBjSession(principal, token, EXPIRES_AT));
    }
  }
}
