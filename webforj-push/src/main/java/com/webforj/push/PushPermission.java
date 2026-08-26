package com.webforj.push;

/**
 * The decision of the user on notifications from the application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum PushPermission {

  /**
   * The user allowed notifications.
   */
  GRANTED("granted"),

  /**
   * The user blocked notifications, the browser shows no further prompt.
   */
  DENIED("denied"),

  /**
   * The user has not decided yet, the next subscribe shows the prompt.
   */
  PROMPT("default");

  private final String value;

  PushPermission(String value) {
    this.value = value;
  }

  /**
   * Returns the permission for the value the browser reports.
   *
   * @param value the value the browser reports
   * @return the permission, {@link #PROMPT} when the value is unknown
   */
  public static PushPermission fromValue(String value) {
    for (PushPermission permission : values()) {
      if (permission.value.equals(value)) {
        return permission;
      }
    }

    return PROMPT;
  }

  /**
   * Returns the value the browser reports for this permission.
   *
   * @return the browser value
   */
  public String getValue() {
    return value;
  }
}
