package com.webforj.plugin.foundation;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Resolves the discovery file that carries the development watch socket port.
 *
 * <p>
 * The watch grabs a free port, listens on it, and writes the port number into this file. The
 * application reads the same file on start to learn where to connect. The file lives in the
 * operating system temporary directory, never under {@code target}, so a clean or an IDE rebuild
 * never removes it, and its name is keyed by the absolute project path so two projects never share
 * a file and never collide.
 * </p>
 *
 * <p>
 * The watch and the application both resolve the file through this one class, so they normalize the
 * absolute project path and hash it identically and reach the same file without sharing any state.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class WatchPortFile {

  private WatchPortFile() {}

  /**
   * Resolves the port file for the given project root.
   *
   * @param projectAbsolutePath the absolute project path
   * @return the port file path
   */
  public static Path resolve(String projectAbsolutePath) {
    String normalized = Path.of(projectAbsolutePath).toAbsolutePath().normalize().toString();
    String key = hash(normalized).substring(0, 16);

    return Path.of(System.getProperty("java.io.tmpdir")).resolve("webforj-watch-" + key + ".port");
  }

  private static String hash(String value) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] bytes = digest.digest(value.getBytes(StandardCharsets.UTF_8));

      StringBuilder hex = new StringBuilder(bytes.length * 2);
      for (byte b : bytes) {
        hex.append(Character.forDigit((b >> 4) & 0xF, 16));
        hex.append(Character.forDigit(b & 0xF, 16));
      }

      return hex.toString();
    } catch (NoSuchAlgorithmException e) {
      throw new UncheckedIOException(new IOException("SHA-256 is not available", e));
    }
  }
}
