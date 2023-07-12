package org.dwcj.component;

/**
 * Supported themes as defined by the BBj Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum Theme implements ThemeBase {
  /* The danger theme as defined by the BBj Theme Engine. */
  DANGER("danger"),
  /* The default theme as defined by the BBj Theme Engine. */
  DEFAULT("default"),
  /* The gray theme as defined by the BBj Theme Engine. */
  GRAY("gray"),
  /* The info theme as defined by the BBj Theme Engine. */
  INFO("info"),
  /* The primary theme as defined by the BBj Theme Engine. */
  PRIMARY("primary"),
  /* The success theme as defined by the BBj Theme Engine. */
  SUCCESS("success"),
  /* The warning theme as defined by the BBj Theme Engine. */
  WARNING("warning");

  private final String value;

  Theme(String value) {
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
