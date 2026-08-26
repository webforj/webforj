package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class PushUrgencyTest {

  @Test
  void shouldExposeTheProtocolValues() {
    assertEquals("very-low", PushUrgency.VERY_LOW.getValue());
    assertEquals("low", PushUrgency.LOW.getValue());
    assertEquals("normal", PushUrgency.NORMAL.getValue());
    assertEquals("high", PushUrgency.HIGH.getValue());
  }
}
