package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class PushKeysTest {

  @Test
  void shouldKeepBothKeys() {
    PushKeys keys = new PushKeys("public", "private");

    assertEquals("public", keys.getPublicKey());
    assertEquals("private", keys.getPrivateKey());
  }

  @Test
  void shouldRejectBlankPublicKey() {
    assertThrows(IllegalArgumentException.class, () -> new PushKeys("", "private"));
  }

  @Test
  void shouldRejectMissingPrivateKey() {
    assertThrows(IllegalArgumentException.class, () -> new PushKeys("public", null));
  }

  @Test
  void shouldHideThePrivateKeyInToString() {
    String text = new PushKeys("public", "secret-value").toString();

    assertTrue(text.contains("public"), "the public key is shown");
    assertFalse(text.contains("secret-value"), "the private key is hidden");
  }
}
