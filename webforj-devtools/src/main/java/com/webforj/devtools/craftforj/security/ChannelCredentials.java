package com.webforj.devtools.craftforj.security;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;

/**
 * The per-page secrets that bind the craftforJ action channel to the client the server handed them
 * to.
 *
 * <p>
 * The nonce travels with every request and never becomes a property of any object the page can
 * reach, so a script that dispatches the request event blindly cannot produce a request the server
 * accepts. The sink id names the response function the server calls, which lets the client install
 * that function under a name no script can predict before the page is served.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ChannelCredentials {

  private static final String ALPHABET =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  private static final int TOKEN_LENGTH = 32;
  private static final SecureRandom RANDOM = new SecureRandom();

  private final String nonce;
  private final String sinkId;

  private ChannelCredentials(String nonce, String sinkId) {
    this.nonce = nonce;
    this.sinkId = sinkId;
  }

  /**
   * Creates credentials from two independently drawn random tokens.
   *
   * @return the new credentials
   */
  public static ChannelCredentials create() {
    return new ChannelCredentials(randomToken(), randomToken());
  }

  /**
   * Creates credentials with the given tokens.
   *
   * @param nonce the request nonce
   * @param sinkId the response sink id
   * @return the new credentials
   */
  public static ChannelCredentials of(String nonce, String sinkId) {
    return new ChannelCredentials(nonce, sinkId);
  }

  /**
   * Gets the nonce every request has to carry.
   *
   * @return the nonce
   */
  public String getNonce() {
    return nonce;
  }

  /**
   * Gets the id that names the response sink on the client.
   *
   * @return the sink id
   */
  public String getSinkId() {
    return sinkId;
  }

  /**
   * Checks a nonce presented by a request.
   *
   * @param presented the nonce carried by the request, may be {@code null}
   * @return {@code true} when the nonce matches
   */
  public boolean matches(String presented) {
    if (presented == null) {
      return false;
    }

    return MessageDigest.isEqual(nonce.getBytes(StandardCharsets.UTF_8),
        presented.getBytes(StandardCharsets.UTF_8));
  }

  private static String randomToken() {
    StringBuilder token = new StringBuilder(TOKEN_LENGTH);
    for (int i = 0; i < TOKEN_LENGTH; i++) {
      token.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
    }

    return token.toString();
  }
}
