package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class PushSubscriptionTest {

  @Test
  void shouldKeepTheThreeValues() {
    PushSubscription subscription =
        new PushSubscription("https://push.example/abc", "p256dh-key", "auth-secret");

    assertEquals("https://push.example/abc", subscription.getEndpoint());
    assertEquals("p256dh-key", subscription.getP256dh());
    assertEquals("auth-secret", subscription.getAuth());
  }

  @Test
  void shouldRejectBlankEndpoint() {
    IllegalArgumentException e = assertThrows(IllegalArgumentException.class,
        () -> new PushSubscription(" ", "p256dh-key", "auth-secret"));

    assertEquals("The subscription endpoint is required", e.getMessage());
  }

  @Test
  void shouldRejectMissingP256dh() {
    assertThrows(IllegalArgumentException.class,
        () -> new PushSubscription("https://push.example/abc", null, "auth-secret"));
  }

  @Test
  void shouldRejectMissingAuth() {
    assertThrows(IllegalArgumentException.class,
        () -> new PushSubscription("https://push.example/abc", "p256dh-key", ""));
  }
}
