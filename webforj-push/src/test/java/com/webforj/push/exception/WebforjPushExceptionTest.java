package com.webforj.push.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.push.PushStatus;
import org.junit.jupiter.api.Test;

class WebforjPushExceptionTest {

  @Test
  void shouldCarryStatusAndMessage() {
    WebforjPushException e = new WebforjPushException(PushStatus.PERMISSION_DENIED, "blocked");

    assertEquals(PushStatus.PERMISSION_DENIED, e.getStatus());
    assertEquals("blocked", e.getMessage());
    assertEquals(0, e.getStatusCode());
    assertNull(e.getCause());
  }

  @Test
  void shouldCarryTheCause() {
    RuntimeException cause = new RuntimeException("root");
    WebforjPushException e = new WebforjPushException(PushStatus.UNREACHABLE, "down", cause);

    assertSame(cause, e.getCause());
    assertEquals(PushStatus.UNREACHABLE, e.getStatus());
  }

  @Test
  void shouldCarryTheAnswerOfThePushService() {
    WebforjPushException e = new WebforjPushException(PushStatus.REJECTED, 429, "too many", null);

    assertEquals(PushStatus.REJECTED, e.getStatus());
    assertEquals(429, e.getStatusCode());
  }

  @Test
  void shouldFallBackToUnknownWithoutTheStatus() {
    assertEquals(PushStatus.UNKNOWN, new WebforjPushException(null, "x").getStatus());
  }
}
