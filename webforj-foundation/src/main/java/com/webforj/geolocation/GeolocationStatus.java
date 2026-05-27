package com.webforj.geolocation;

/**
 * The status reported by the browser for a {@link Geolocation} request.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public enum GeolocationStatus {

  /**
   * Success.
   */
  SUCCESS(0),

  /**
   * The location acquisition process failed because the application origin does not have permission
   * to use the Geolocation API. This is the value returned if the user refuses to share their
   * location.
   */
  PERMISSION_DENIED(1),

  /**
   * The position of the device could not be determined. For instance, one or more of the location
   * providers used in the location acquisition process reported an internal error that caused the
   * process to fail entirely.
   */
  POSITION_UNAVAILABLE(2),

  /**
   * The length of time specified by the timeout property has elapsed before the browser could
   * successfully acquire a new position.
   */
  TIMEOUT(3),

  /**
   * The status code reported by the browser is not recognized.
   */
  UNKNOWN(-1);

  private final int code;

  GeolocationStatus(int code) {
    this.code = code;
  }

  /**
   * Returns the numeric status code reported by the browser.
   *
   * @return the numeric status code
   */
  public int getCode() {
    return code;
  }

  /**
   * Resolves a status from its numeric code, returning {@link #UNKNOWN} when no status matches.
   *
   * @param code the numeric status code
   * @return the matching status, or {@link #UNKNOWN} when none matches
   */
  public static GeolocationStatus fromCode(int code) {
    for (GeolocationStatus status : values()) {
      if (status.code == code) {
        return status;
      }
    }

    return UNKNOWN;
  }
}
