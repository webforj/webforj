package com.webforj.push;

import com.typesafe.config.Config;
import com.webforj.push.exception.WebforjPushException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The push settings of a deployment, read from the {@code webforj.push} configuration keys.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushConfiguration {

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

  private static final String KEYS_COMMAND = "webforj:push-keys";

  private final PushKeys keys;
  private final String contact;

  /**
   * Creates a configuration.
   *
   * @param keys the key pair the deployment signs its pushes with
   * @param subject the contact of the deployment, a {@code mailto:} or {@code https://} address the
   *        push services may reach the operator on
   *
   * @throws WebforjPushException when the keys are missing or the subject is not a {@code mailto:}
   *         or {@code https://} address
   */
  public PushConfiguration(PushKeys keys, String subject) {
    if (keys == null) {
      throw new WebforjPushException(PushStatus.NOT_CONFIGURED, "The push keys are required");
    }

    if (subject == null || !(subject.startsWith("mailto:") || subject.startsWith("https://"))) {
      throw new WebforjPushException(PushStatus.NOT_CONFIGURED,
          "The push subject must be a mailto: or https:// address, set " + SUBJECT
              + " accordingly");
    }

    this.keys = keys;
    this.contact = subject;
  }

  /**
   * Returns the key pair the deployment signs its pushes with.
   *
   * @return the keys
   */
  public PushKeys getKeys() {
    return keys;
  }

  /**
   * Returns the contact of the deployment.
   *
   * @return the subject, a {@code mailto:} or {@code https://} address
   */
  public String getSubject() {
    return contact;
  }

  /**
   * Reads the configuration from the given config.
   *
   * @param config the config to read
   * @return the configuration, empty when none of the push keys is set
   *
   * @throws WebforjPushException when only some of the push keys are set
   */
  public static Optional<PushConfiguration> fromConfig(Config config) {
    String publicKey = value(config, PUBLIC_KEY);
    String privateKey = value(config, PRIVATE_KEY);
    String subject = value(config, SUBJECT);

    if (publicKey == null && privateKey == null && subject == null) {
      return Optional.empty();
    }

    List<String> missing = new ArrayList<>();
    if (publicKey == null) {
      missing.add(PUBLIC_KEY);
    }

    if (privateKey == null) {
      missing.add(PRIVATE_KEY);
    }

    if (subject == null) {
      missing.add(SUBJECT);
    }

    if (!missing.isEmpty()) {
      throw new WebforjPushException(PushStatus.NOT_CONFIGURED,
          "Push is partially configured, missing " + String.join(", ", missing) + ". Run "
              + KEYS_COMMAND + " to generate the keys and set all three keys");
    }

    return Optional.of(new PushConfiguration(new PushKeys(publicKey, privateKey), subject));
  }

  /**
   * Reads the configuration from the given config, failing when push is not configured.
   *
   * @param config the config to read
   * @return the configuration
   *
   * @throws WebforjPushException when push is not or only partially configured
   */
  public static PushConfiguration require(Config config) {
    return fromConfig(config).orElseThrow(() -> new WebforjPushException(PushStatus.NOT_CONFIGURED,
        "Push is not configured. Run " + KEYS_COMMAND + " to generate the keys and set "
            + PUBLIC_KEY + ", " + PRIVATE_KEY + " and " + SUBJECT));
  }

  private static String value(Config config, String key) {
    if (config == null || !config.hasPath(key) || config.getIsNull(key)) {
      return null;
    }

    String value = config.getString(key);
    return value.isBlank() ? null : value.trim();
  }
}
