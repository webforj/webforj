package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

class BBjSecurityErrorTest {

  @Test
  void shouldKeepTheMessageAndTheCause() {
    Exception cause = new IllegalStateException("broken");

    BBjSecurityError error = new BBjSecurityError("not registered", cause);

    assertEquals("not registered", error.getMessage());
    assertSame(cause, error.getCause());
  }

  @Test
  void shouldBeAnError() {
    assertInstanceOf(Error.class, new BBjSecurityError("not registered", null));
  }
}
