package com.webforj.devtools.craftforj.module.model;

/**
 * One craftforJ client module, base64 encoded and paired with its digest.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ModuleSource {

  private final String base64;
  private final String sha256;

  /**
   * Creates a module source.
   *
   * @param base64 the base64 encoded module
   * @param sha256 the SHA-256 hex digest of the base64 payload
   */
  public ModuleSource(String base64, String sha256) {
    this.base64 = base64;
    this.sha256 = sha256;
  }

  /**
   * Gets the base64 encoded module.
   *
   * @return the base64 payload
   */
  public String getBase64() {
    return base64;
  }

  /**
   * Gets the digest of the payload.
   *
   * @return the SHA-256 hex digest
   */
  public String getSha256() {
    return sha256;
  }
}
