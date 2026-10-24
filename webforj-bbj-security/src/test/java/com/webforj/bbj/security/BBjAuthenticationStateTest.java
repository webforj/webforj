package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Instant;
import java.util.List;
import org.junit.jupiter.api.Test;

class BBjAuthenticationStateTest {
  private final BBjAuthenticationState state = new BBjAuthenticationState();
  private final BBjAuthentication authentication =
      new BBjAuthentication("alice", List.of("Admins"), Instant.parse("2026-01-01T00:00:00Z"));

  @Test
  void shouldStartEmpty() {
    assertTrue(state.getAuthentication().isEmpty());
  }

  @Test
  void shouldHoldTheSetAuthentication() {
    state.setAuthentication(authentication);

    assertSame(authentication, state.getAuthentication().orElseThrow());
  }

  @Test
  void shouldReplaceThePreviousAuthentication() {
    BBjAuthentication other =
        new BBjAuthentication("bob", List.of(), Instant.parse("2026-02-01T00:00:00Z"));
    state.setAuthentication(authentication);

    state.setAuthentication(other);

    assertSame(other, state.getAuthentication().orElseThrow());
  }

  @Test
  void shouldBeEmptyAfterClearing() {
    state.setAuthentication(authentication);

    state.clearAuthentication();

    assertTrue(state.getAuthentication().isEmpty());
  }

  @Test
  void shouldRefuseANullAuthentication() {
    assertThrows(NullPointerException.class, () -> state.setAuthentication(null));
  }
}
