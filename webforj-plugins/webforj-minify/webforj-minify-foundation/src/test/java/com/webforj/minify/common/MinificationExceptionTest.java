package com.webforj.minify.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Unit tests for MinificationException.
 *
 * @author Kevin Hagel
 */
class MinificationExceptionTest {

  @Test
  void testConstructorWithMessage() {
    String message = "Test error message";
    MinificationException exception = new MinificationException(message);

    assertEquals(message, exception.getMessage());
    assertNull(exception.getCause());
  }

  @Test
  void testConstructorWithMessageAndCause() {
    String message = "Test error message";
    Throwable cause = new RuntimeException("Original cause");
    MinificationException exception = new MinificationException(message, cause);

    assertEquals(message, exception.getMessage());
    assertEquals(cause, exception.getCause());
  }

  @Test
  void testThrowAndCatch() {
    assertThrows(MinificationException.class, () -> {
      throw new MinificationException("Test exception");
    });
  }

  @Test
  void testExceptionWithNullMessage() {
    MinificationException exception = new MinificationException(null);
    assertNull(exception.getMessage());
  }

  @Test
  void testExceptionWithNullCause() {
    MinificationException exception = new MinificationException("Message", null);
    assertEquals("Message", exception.getMessage());
    assertNull(exception.getCause());
  }


  @Test
  void testStackTrace() {
    MinificationException exception = new MinificationException("Test");
    assertNotNull(exception.getStackTrace());
    assertTrue(exception.getStackTrace().length > 0);
  }
}
