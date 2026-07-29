package com.webforj.devtools.craftforj.keys;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.spec.ECGenParameterSpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Base64;
import javax.crypto.Cipher;
import javax.crypto.KeyAgreement;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * End-to-end encryption for secrets travelling over the craftforJ action channel.
 *
 * <p>
 * The transport holds one P-256 key pair for the lifetime of the JVM. A client sends its ephemeral
 * public key with each request and both sides derive a shared AES-256-GCM key via ECDH and a
 * SHA-256 digest of the shared secret.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class KeyTransport {

  private static final int GCM_TAG_BITS = 128;
  private static final int IV_BYTES = 12;

  private final KeyPair keyPair;
  private final SecureRandom random = new SecureRandom();

  /**
   * Creates a transport with a fresh P-256 key pair.
   */
  public KeyTransport() {
    try {
      KeyPairGenerator generator = KeyPairGenerator.getInstance("EC");
      generator.initialize(new ECGenParameterSpec("secp256r1"));
      this.keyPair = generator.generateKeyPair();
    } catch (GeneralSecurityException e) {
      throw new IllegalStateException("Failed to create craftforJ key transport", e);
    }
  }

  /**
   * Gets this server's public key.
   *
   * @return the SPKI-encoded public key, base64
   */
  public String getPublicKey() {
    return Base64.getEncoder().encodeToString(keyPair.getPublic().getEncoded());
  }

  /**
   * Encrypts a payload for the given client.
   *
   * @param clientPublicKey the client's SPKI-encoded public key, base64
   * @param plaintext the payload to protect
   * @return the sealed payload
   */
  public Sealed encrypt(String clientPublicKey, String plaintext) {
    try {
      byte[] iv = new byte[IV_BYTES];
      random.nextBytes(iv);
      Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
      cipher.init(Cipher.ENCRYPT_MODE, deriveKey(clientPublicKey),
          new GCMParameterSpec(GCM_TAG_BITS, iv));
      byte[] payload = cipher.doFinal(plaintext.getBytes(StandardCharsets.UTF_8));
      Base64.Encoder encoder = Base64.getEncoder();

      return new Sealed(encoder.encodeToString(iv), encoder.encodeToString(payload));
    } catch (GeneralSecurityException | IllegalArgumentException e) {
      throw new IllegalStateException("Failed to encrypt payload", e);
    }
  }

  /**
   * Decrypts a payload sealed by the given client.
   *
   * @param clientPublicKey the client's SPKI-encoded public key, base64
   * @param iv the GCM initialization vector, base64
   * @param payload the ciphertext, base64
   * @return the plaintext
   */
  public String decrypt(String clientPublicKey, String iv, String payload) {
    try {
      Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
      cipher.init(Cipher.DECRYPT_MODE, deriveKey(clientPublicKey),
          new GCMParameterSpec(GCM_TAG_BITS, Base64.getDecoder().decode(iv)));
      byte[] plaintext = cipher.doFinal(Base64.getDecoder().decode(payload));

      return new String(plaintext, StandardCharsets.UTF_8);
    } catch (GeneralSecurityException | IllegalArgumentException e) {
      throw new IllegalStateException("Failed to decrypt payload", e);
    }
  }

  private SecretKeySpec deriveKey(String clientPublicKey) throws GeneralSecurityException {
    KeyFactory factory = KeyFactory.getInstance("EC");
    PublicKey clientKey =
        factory.generatePublic(new X509EncodedKeySpec(Base64.getDecoder().decode(clientPublicKey)));
    KeyAgreement agreement = KeyAgreement.getInstance("ECDH");
    agreement.init(keyPair.getPrivate());
    agreement.doPhase(clientKey, true);
    byte[] digest = MessageDigest.getInstance("SHA-256").digest(agreement.generateSecret());

    return new SecretKeySpec(digest, "AES");
  }

  /**
   * An encrypted payload with its initialization vector.
   */
  public static class Sealed {

    private final String iv;
    private final String payload;

    Sealed(String iv, String payload) {
      this.iv = iv;
      this.payload = payload;
    }

    /**
     * Gets the GCM initialization vector.
     *
     * @return the iv, base64
     */
    public String getIv() {
      return iv;
    }

    /**
     * Gets the ciphertext.
     *
     * @return the payload, base64
     */
    public String getPayload() {
      return payload;
    }
  }
}
