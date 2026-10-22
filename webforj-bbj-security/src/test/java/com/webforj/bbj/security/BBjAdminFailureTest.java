package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.bbj.security.BBjAdminFailure.Kind;
import org.junit.jupiter.api.Test;

class BBjAdminFailureTest {

  @Test
  void shouldExposeTheKindAndTheMessage() {
    BBjAdminFailure failure = new BBjAdminFailure(Kind.TOKEN_REJECTED, "refused");

    assertEquals(Kind.TOKEN_REJECTED, failure.getKind());
    assertEquals("refused", failure.getMessage());
  }

  @Test
  void shouldHaveNoCauseAndNoSuppressedExceptions() {
    BBjAdminFailure failure = new BBjAdminFailure(Kind.UNAVAILABLE, "down");

    failure.addSuppressed(new IllegalStateException("secret"));

    assertNull(failure.getCause());
    assertEquals(0, failure.getSuppressed().length);
  }

  @Test
  void shouldRefuseANullKind() {
    assertThrows(NullPointerException.class, () -> new BBjAdminFailure(null, "refused"));
  }

  @Test
  void shouldRefuseANullMessage() {
    assertThrows(NullPointerException.class, () -> new BBjAdminFailure(Kind.UNAVAILABLE, null));
  }
}
