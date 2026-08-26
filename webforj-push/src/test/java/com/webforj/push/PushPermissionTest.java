package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class PushPermissionTest {

  @Test
  void shouldMapTheBrowserValues() {
    assertEquals(PushPermission.GRANTED, PushPermission.fromValue("granted"));
    assertEquals(PushPermission.DENIED, PushPermission.fromValue("denied"));
    assertEquals(PushPermission.PROMPT, PushPermission.fromValue("default"));
  }

  @Test
  void shouldFallBackToPromptForUnknownValues() {
    assertEquals(PushPermission.PROMPT, PushPermission.fromValue("weird"));
    assertEquals(PushPermission.PROMPT, PushPermission.fromValue(null));
  }

  @Test
  void shouldExposeTheBrowserValue() {
    assertEquals("default", PushPermission.PROMPT.getValue());
    assertEquals("granted", PushPermission.GRANTED.getValue());
  }
}
