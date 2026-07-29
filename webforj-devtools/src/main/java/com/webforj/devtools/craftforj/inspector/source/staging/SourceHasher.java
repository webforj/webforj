package com.webforj.devtools.craftforj.inspector.source.staging;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Computes the content hash used to detect concurrent edits between read and write.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SourceHasher {

  private SourceHasher() {}

  /**
   * Hashes source content with SHA-256.
   *
   * @param content the source content
   * @return the lowercase hex digest
   */
  public static String hash(String content) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] bytes = digest.digest(content.getBytes(StandardCharsets.UTF_8));
      StringBuilder hex = new StringBuilder(bytes.length * 2);
      for (byte b : bytes) {
        hex.append(Character.forDigit((b >> 4) & 0xF, 16));
        hex.append(Character.forDigit(b & 0xF, 16));
      }

      return hex.toString();
    } catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException("SHA-256 is not available", e);
    }
  }
}
