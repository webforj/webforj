package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class PushStatusTest {

  @Test
  void shouldNameEveryFailureReason() {
    assertEquals(7, PushStatus.values().length);
    assertEquals(PushStatus.SUBSCRIPTION_EXPIRED, PushStatus.valueOf("SUBSCRIPTION_EXPIRED"));
  }
}
