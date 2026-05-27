package com.webforj.geolocation.exception;

import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.geolocation.GeolocationStatus;

/**
 * Reports a failure returned by the browser when requesting the device's geographic position.
 *
 * <p>
 * The exception carries the {@link GeolocationStatus} reported by the browser together with the
 * additional information the browser provided about the failure, when any was provided.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class WebforjGeolocationException extends WebforjRuntimeException {

  private final transient GeolocationStatus status;

  /**
   * Constructs a new exception with the given status and detail message.
   *
   * @param status the status reported by the browser
   * @param message additional information about the failure
   */
  public WebforjGeolocationException(GeolocationStatus status, String message) {
    super(message);
    this.status = status;
  }

  /**
   * Constructs a new exception with the given status, detail message, and cause.
   *
   * @param status the status reported by the browser
   * @param message additional information about the failure
   * @param cause the underlying cause
   */
  public WebforjGeolocationException(GeolocationStatus status, String message, Throwable cause) {
    super(message, cause);
    this.status = status;
  }

  /**
   * Returns the status reported by the browser.
   *
   * @return the status
   */
  public GeolocationStatus getStatus() {
    return status;
  }
}
