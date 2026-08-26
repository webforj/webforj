package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.interaso.webpush.VapidKeys;
import com.webforj.push.exception.WebforjPushException;
import java.util.Base64;
import org.junit.jupiter.api.Test;

class VapidKeyAdapterTest {

  @Test
  void shouldConvertTheConfiguredStringsBackIntoTheSamePair() {
    VapidKeys generated = VapidKeys.generate();
    PushKeys keys = new PushKeys(generated.getX509PublicKey(), generated.getPkcs8PrivateKey());

    VapidKeys converted = VapidKeyAdapter.toVapidKeys(keys);

    assertArrayEquals(generated.getApplicationServerKey(), converted.getApplicationServerKey());
  }

  @Test
  void shouldRenderTheApplicationServerKeyAsBase64UrlWithoutPadding() {
    VapidKeys generated = VapidKeys.generate();
    PushKeys keys = new PushKeys(generated.getX509PublicKey(), generated.getPkcs8PrivateKey());

    String key = VapidKeyAdapter.toApplicationServerKey(keys);
    byte[] raw = Base64.getUrlDecoder().decode(key);

    assertEquals(65, raw.length, "an uncompressed P-256 point");
    assertEquals(4, raw[0], "the uncompressed point marker");
    assertFalse(key.contains("="), "no padding");
    assertFalse(key.contains("+") || key.contains("/"), "url safe alphabet");
  }

  @Test
  void shouldFailNamingTheCommandWhenTheKeysAreMalformed() {
    PushKeys malformed = new PushKeys("not-a-key", "not-a-key");
    WebforjPushException e =
        assertThrows(WebforjPushException.class, () -> VapidKeyAdapter.toVapidKeys(malformed));

    assertTrue(e.getMessage().contains("webforj:push-keys"), e.getMessage());
    assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
  }

  @Test
  void shouldFailWhenTheKeysDoNotFormThePair() {
    VapidKeys first = VapidKeys.generate();
    VapidKeys second = VapidKeys.generate();

    PushKeys mismatched = new PushKeys(first.getX509PublicKey(), second.getPkcs8PrivateKey());
    assertThrows(WebforjPushException.class, () -> VapidKeyAdapter.toVapidKeys(mismatched));
  }
}
