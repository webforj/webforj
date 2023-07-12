package org.dwcj.component.button;

import org.dwcj.component.ThemeBase;

/**
 * Supported button themes as defined by the BBj Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum ButtonTheme implements ThemeBase {
  /**
   * The danger theme as defined by the BBj Theme Engine.
   **/
  DANGER("danger"),
  /**
   * The outlined-danger theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_DANGER("outlined-danger"),
  /**
   * The default theme as defined by the BBj Theme Engine.
   **/
  DEFAULT("default"),
  /**
   * The outlined-default theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_DEFAULT("outlined-default"),
  /**
   * The gray theme as defined by the BBj Theme Engine.
   **/
  GRAY("gray"),
  /**
   * The outlined-gray theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_GRAY("outlined-gray"),
  /**
   * The info theme as defined by the BBj Theme Engine.
   **/
  INFO("info"),
  /**
   * The outlined-info theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_INFO("outlined-info"),
  /**
   * The primary theme as defined by the BBj Theme Engine.
   **/
  PRIMARY("primary"),
  /**
   * The outlined-primary theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_PRIMARY("outlined-primary"),
  /**
   * The success theme as defined by the BBj Theme Engine.
   **/
  SUCCESS("success"),
  /**
   * The outlined-success theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_SUCCESS("outlined-success"),
  /**
   * The warning theme as defined by the BBj Theme Engine.
   **/
  WARNING("warning"),
  /**
   * The outlined-warning theme as defined by the BBj Theme Engine.
   **/
  OUTLINED_WARNING("outlined-warning");

  private final String value;

  ButtonTheme(String value) {
    this.value = value;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getValue() {
    return this.value;
  }
}
