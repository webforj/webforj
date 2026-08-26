package com.webforj.push.exception;

import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.push.PushStatus;

/**
 * Thrown when a push operation fails, carrying the reason as a {@link PushStatus}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class WebforjPushException extends WebforjRuntimeException {

  private final transient PushStatus status;
  private final int statusCode;

  /**
   * Constructs a new exception with the given status and detail message.
   *
   * @param status the reason of the failure
   * @param message the detail message
   */
  public WebforjPushException(PushStatus status, String message) {
    this(status, message, null);
  }

  /**
   * Constructs a new exception with the given status, detail message, and cause.
   *
   * @param status the reason of the failure
   * @param message the detail message
   * @param cause the underlying cause
   */
  public WebforjPushException(PushStatus status, String message, Throwable cause) {
    this(status, 0, message, cause);
  }

  /**
   * Constructs a new exception for an answer of a push service.
   *
   * @param status the reason of the failure
   * @param statusCode the HTTP status the push service answered
   * @param message the detail message
   * @param cause the underlying cause
   */
  public WebforjPushException(PushStatus status, int statusCode, String message, Throwable cause) {
    super(message, cause);
    this.status = status == null ? PushStatus.UNKNOWN : status;
    this.statusCode = statusCode;
  }

  /**
   * Returns the reason of the failure.
   *
   * @return the status
   */
  public PushStatus getStatus() {
    return status;
  }

  /**
   * Returns the HTTP status a push service answered when the failure came from a delivery.
   *
   * @return the status code, {@code 0} when the failure is not an answer of a push service
   */
  public int getStatusCode() {
    return statusCode;
  }
}
