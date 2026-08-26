package com.webforj.push;

import java.io.Serializable;

/**
 * The key pair a deployment signs its pushes with, as printed by the webforJ build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushKeys implements Serializable {

  private final String publicKey;
  private final String privateKey;

  /**
   * Creates a key pair.
   *
   * @param publicKey the public key, base64 encoded X509
   * @param privateKey the private key, base64 encoded PKCS8
   *
   * @throws IllegalArgumentException when either key is null or blank
   */
  public PushKeys(String publicKey, String privateKey) {
    if (publicKey == null || publicKey.isBlank()) {
      throw new IllegalArgumentException("The public key is required");
    }

    if (privateKey == null || privateKey.isBlank()) {
      throw new IllegalArgumentException("The private key is required");
    }

    this.publicKey = publicKey;
    this.privateKey = privateKey;
  }

  /**
   * Returns the public key, base64 encoded X509.
   *
   * @return the public key
   */
  public String getPublicKey() {
    return publicKey;
  }

  /**
   * Returns the private key, base64 encoded PKCS8.
   *
   * @return the private key
   */
  public String getPrivateKey() {
    return privateKey;
  }

  @Override
  public String toString() {
    return "PushKeys[publicKey=" + publicKey + ", privateKey=***]";
  }
}
