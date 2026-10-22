package com.webforj.bbj.security;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.Optional;
import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Encrypts a login session into the text of one token store entry and reads it back.
 *
 * <p>
 * The text is the Base64 URL form of a random initialization vector followed by the AES-256-GCM
 * ciphertext. The name of the entry is authenticated with it, so a text moved to another entry name
 * cannot be read.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjTokenEntryCipher {
  private static final String TRANSFORMATION = "AES/GCM/NoPadding";
  private static final int RANDOM_BYTES = 32;
  private static final int IV_BYTES = 12;
  private static final int TAG_BITS = 128;
  private static final int TAG_BYTES = TAG_BITS / 8;
  private final SecureRandom random = new SecureRandom();

  /**
   * Creates a random handle for an entry name.
   *
   * @return 43 URL safe characters
   */
  String createHandle() {
    return createRandomText();
  }

  /**
   * Creates a random key for one entry.
   *
   * @return 43 URL safe characters
   */
  String createKey() {
    return createRandomText();
  }

  /**
   * Encrypts a session into the text of an entry.
   *
   * @param entryName the name of the entry, authenticated with the text
   * @param key the key made by {@link #createKey()}
   * @param session the session to encrypt
   * @return the text of the entry
   *
   * @throws IllegalStateException if the session cannot be encrypted, the message holds no secret
   *         and there is no cause
   */
  String formatEntry(String entryName, String key, BBjLoginSession session) {
    byte[] plain = null;
    byte[] keyBytes = null;

    try {
      keyBytes = Base64.getUrlDecoder().decode(key);
      plain = writePlain(session);
      byte[] iv = new byte[IV_BYTES];
      random.nextBytes(iv);
      Cipher cipher = Cipher.getInstance(TRANSFORMATION);
      cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(keyBytes, "AES"),
          new GCMParameterSpec(TAG_BITS, iv));
      cipher.updateAAD(entryName.getBytes(StandardCharsets.UTF_8));
      byte[] encrypted = cipher.doFinal(plain);
      byte[] whole = new byte[IV_BYTES + encrypted.length];
      System.arraycopy(iv, 0, whole, 0, IV_BYTES);
      System.arraycopy(encrypted, 0, whole, IV_BYTES, encrypted.length);

      return Base64.getUrlEncoder().withoutPadding().encodeToString(whole);
    } catch (GeneralSecurityException | IOException | RuntimeException e) {
      throw new IllegalStateException("The sign-in could not be encrypted");
    } finally {
      clearBytes(plain);
      clearBytes(keyBytes);
    }
  }

  /**
   * Decrypts the text of an entry.
   *
   * @param entryName the name of the entry the text was made for
   * @param key the key the text was made with
   * @param entry the text of the entry
   * @return the session, or empty when the key, the name or the text is wrong or the text was
   *         changed
   */
  Optional<BBjLoginSession> parseEntry(String entryName, String key, String entry) {
    byte[] plain = null;
    byte[] keyBytes = null;

    try {
      keyBytes = Base64.getUrlDecoder().decode(key);
      byte[] whole = Base64.getUrlDecoder().decode(entry);
      if (whole.length < IV_BYTES + TAG_BYTES) {
        return Optional.empty();
      }

      Cipher cipher = Cipher.getInstance(TRANSFORMATION);
      cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(keyBytes, "AES"),
          new GCMParameterSpec(TAG_BITS, whole, 0, IV_BYTES));
      cipher.updateAAD(entryName.getBytes(StandardCharsets.UTF_8));
      plain = cipher.doFinal(whole, IV_BYTES, whole.length - IV_BYTES);

      return Optional.of(readPlain(plain));
    } catch (GeneralSecurityException | IOException | RuntimeException e) {
      return Optional.empty();
    } finally {
      clearBytes(plain);
      clearBytes(keyBytes);
    }
  }

  private String createRandomText() {
    byte[] bytes = new byte[RANDOM_BYTES];
    random.nextBytes(bytes);
    String text = Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    clearBytes(bytes);

    return text;
  }

  private static byte[] writePlain(BBjLoginSession session) throws IOException {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (DataOutputStream out = new DataOutputStream(bytes)) {
      out.writeLong(session.getExpiresAt().toEpochMilli());
      out.writeUTF(session.getUserName());
      out.writeUTF(session.getToken());
    }

    return bytes.toByteArray();
  }

  private static BBjLoginSession readPlain(byte[] plain) throws IOException {
    try (DataInputStream in = new DataInputStream(new ByteArrayInputStream(plain))) {
      Instant expiresAt = Instant.ofEpochMilli(in.readLong());
      String userName = in.readUTF();

      return new BBjLoginSession(userName, in.readUTF(), expiresAt);
    }
  }

  private static void clearBytes(byte[] bytes) {
    if (bytes != null) {
      Arrays.fill(bytes, (byte) 0);
    }
  }
}
