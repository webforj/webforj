package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

class PushActionTest {

  @Test
  void shouldKeepTheValues() {
    PushAction action = new PushAction("track", "Track", "/orders/1/tracking");

    assertEquals("track", action.getAction());
    assertEquals("Track", action.getTitle());
    assertEquals("/orders/1/tracking", action.getUrl());
  }

  @Test
  void shouldAllowMissingUrl() {
    assertNull(new PushAction("dismiss", "Dismiss", null).getUrl());
  }

  @Test
  void shouldRejectBlankAction() {
    assertThrows(IllegalArgumentException.class, () -> new PushAction(" ", "Track", null));
  }

  @Test
  void shouldRejectBlankTitle() {
    assertThrows(IllegalArgumentException.class, () -> new PushAction("track", "", null));
  }
}
