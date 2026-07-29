package com.webforj.devtools.craftforj.inspector.source.staging;

/**
 * Signals a refused staging operation with a machine readable code.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class StagingException extends RuntimeException {

  private final transient String code;

  /**
   * Creates a staging exception.
   *
   * @param code the machine readable rejection code
   * @param message the human readable message
   */
  public StagingException(String code, String message) {
    super(message);
    this.code = code;
  }

  /**
   * Gets the machine readable rejection code.
   *
   * @return the code
   */
  public String getCode() {
    return code;
  }
}
