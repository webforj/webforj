package com.webforj.plugin.foundation.push;

import com.interaso.webpush.VapidKeys;
import java.util.List;

/**
 * Generates the key pair a deployment signs its pushes with and renders it as the configuration
 * lines the application reads.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushKeyCommand {

  /**
   * The line the rendered configuration starts with, telling the user where the lines go.
   */
  public static final String BANNER = "Paste these lines into webforj.conf, or without the quotes"
      + " into application.properties, then replace the subject with the contact of your"
      + " deployment:";

  /**
   * The configuration key of the public key.
   */
  public static final String PUBLIC_KEY = "webforj.push.public-key";

  /**
   * The configuration key of the private key.
   */
  public static final String PRIVATE_KEY = "webforj.push.private-key";

  /**
   * The configuration key of the subject.
   */
  public static final String SUBJECT = "webforj.push.subject";

  private static final String SUBJECT_PLACEHOLDER = "mailto:you@example.com";

  private PushKeyCommand() {}

  /**
   * Generates a new key pair and renders the configuration lines, the values quoted so the lines
   * parse as HOCON.
   *
   * @return the lines to print, the {@link #BANNER}, an empty line and the three configuration
   *         lines
   */
  public static List<String> render() {
    VapidKeys keys = VapidKeys.generate();

    return List.of(BANNER, "", line(PUBLIC_KEY, keys.getX509PublicKey()),
        line(PRIVATE_KEY, keys.getPkcs8PrivateKey()), line(SUBJECT, SUBJECT_PLACEHOLDER));
  }

  private static String line(String key, String value) {
    return key + " = \"" + value + "\"";
  }
}
