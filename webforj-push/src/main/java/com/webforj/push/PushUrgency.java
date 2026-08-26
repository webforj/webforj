package com.webforj.push;

/**
 * How much a push service should spend to deliver a message promptly, as in the Web Push protocol.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum PushUrgency {

  /**
   * Deliver when the device has power and an unmetered connection.
   */
  VERY_LOW("very-low"),

  /**
   * Deliver when the device has power or an unmetered connection.
   */
  LOW("low"),

  /**
   * Deliver as soon as the device is reachable.
   */
  NORMAL("normal"),

  /**
   * Deliver immediately, for time critical messages such as incoming calls.
   */
  HIGH("high");

  private final String value;

  PushUrgency(String value) {
    this.value = value;
  }

  /**
   * Returns the value the push service protocol uses for this urgency.
   *
   * @return the protocol value
   */
  public String getValue() {
    return value;
  }
}
