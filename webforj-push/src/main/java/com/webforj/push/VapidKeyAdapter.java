package com.webforj.push;

import com.interaso.webpush.VapidKeys;
import com.webforj.push.exception.WebforjPushException;
import java.util.Base64;

/**
 * Converts the configured key strings into the key objects the push protocol library signs with.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class VapidKeyAdapter {

  private VapidKeyAdapter() {}

  /**
   * Converts the given keys.
   *
   * @param keys the configured keys
   * @return the library keys
   *
   * @throws WebforjPushException when the keys are malformed or do not form a pair
   */
  static VapidKeys toVapidKeys(PushKeys keys) {
    try {
      return VapidKeys.create(keys.getPublicKey(), keys.getPrivateKey());
    } catch (Exception e) {
      throw new WebforjPushException(PushStatus.NOT_CONFIGURED,
          "The push keys are not valid, run webforj:push-keys " + "and set "
              + PushConfiguration.PUBLIC_KEY + " and " + PushConfiguration.PRIVATE_KEY
              + " to its output",
          e);
    }
  }

  /**
   * Returns the public key in the form the browser expects on subscribe.
   *
   * @param keys the configured keys
   * @return the application server key, base64url encoded without padding
   */
  static String toApplicationServerKey(PushKeys keys) {
    byte[] raw = toVapidKeys(keys).getApplicationServerKey();
    return Base64.getUrlEncoder().withoutPadding().encodeToString(raw);
  }
}
