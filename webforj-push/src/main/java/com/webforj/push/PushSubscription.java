package com.webforj.push;

import java.io.Serializable;

/**
 * The address of one subscribed browser, handed back by the browser on subscribe and required for
 * every send to it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushSubscription implements Serializable {

  private final String endpoint;
  private final String p256dh;
  private final String auth;

  /**
   * Creates a subscription.
   *
   * @param endpoint the delivery URL the push service of the browser vendor assigned
   * @param p256dh the public key of the browser, base64url encoded
   * @param auth the authentication secret of the browser, base64url encoded
   *
   * @throws IllegalArgumentException when any value is null or blank
   */
  public PushSubscription(String endpoint, String p256dh, String auth) {
    this.endpoint = requireText(endpoint, "endpoint");
    this.p256dh = requireText(p256dh, "p256dh");
    this.auth = requireText(auth, "auth");
  }

  /**
   * Returns the delivery URL the push service of the browser vendor assigned.
   *
   * @return the endpoint
   */
  public String getEndpoint() {
    return endpoint;
  }

  /**
   * Returns the public key of the browser, base64url encoded.
   *
   * @return the p256dh key
   */
  public String getP256dh() {
    return p256dh;
  }

  /**
   * Returns the authentication secret of the browser, base64url encoded.
   *
   * @return the auth secret
   */
  public String getAuth() {
    return auth;
  }

  private static String requireText(String value, String name) {
    if (value == null || value.isBlank()) {
      throw new IllegalArgumentException("The subscription " + name + " is required");
    }

    return value;
  }
}
