package com.webforj.utilities;


/**
 * Class is created in order to streamline some of the BBj-specific methods and implementations that
 * are required for use in DWCJ controls
 */
public final class BBjFunctionalityHelper {

  private BBjFunctionalityHelper() {}

  /**
   * Returns a byte array with the bytes set corresponding with whether or not it should be enabled
   * and/or visible on creation
   *
   * @param visible Boolean control's visibility status
   * @param enabled Boolean control's enabled status
   * @return A byte array used in control initialization
   */
  public static byte[] buildStandardCreationFlags(Boolean visible, Boolean enabled) {

    byte bFlag = (byte) 0x00;

    if (Boolean.FALSE.equals(visible)) {
      bFlag += (byte) 0x10;
    }
    if (Boolean.FALSE.equals(enabled)) {
      bFlag += (byte) 0x01;
    }

    return new byte[] {(byte) 0x00, bFlag};

  }
}
