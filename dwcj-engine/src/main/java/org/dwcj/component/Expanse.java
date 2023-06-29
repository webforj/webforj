package org.dwcj.component;

/**
 * Supported expanses as defined by the BBj Theme Engine.
 *
 * @author Hyyan Abo Fakher
 * @since 23.02
 */
public enum Expanse implements ExpanseBase {
  /* The xlarge expanse as defined by the BBj Theme Engine. */
  XLARGE("xl"),
  /* The large expanse as defined by the BBj Theme Engine. */
  LARGE("l"),
  /* The medium expanse as defined by the BBj Theme Engine. */
  MEDIUM("m"),
  /* The small expanse as defined by the BBj Theme Engine. */
  SMALL("s"),
  /* The xsmall expanse as defined by the BBj Theme Engine. */
  XSMALL("xs");

  private final String value;

  Expanse(String value) {
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
