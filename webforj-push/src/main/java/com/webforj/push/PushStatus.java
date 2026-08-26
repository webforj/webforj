package com.webforj.push;

/**
 * The reason a push operation failed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum PushStatus {

  /**
   * The user blocked notifications from the application in the browser.
   */
  PERMISSION_DENIED,

  /**
   * The browser cannot receive pushes, because it lacks the capability or the page is not served
   * from a secure context.
   */
  UNSUPPORTED,

  /**
   * The deployment does not configure push, or configures it only partially.
   */
  NOT_CONFIGURED,

  /**
   * The push service no longer knows the subscription, the application should delete it.
   */
  SUBSCRIPTION_EXPIRED,

  /**
   * The push service refused the message, the status code names the answer.
   */
  REJECTED,

  /**
   * The push service could not be reached.
   */
  UNREACHABLE,

  /**
   * The browser or the keys reported a failure the status does not classify.
   */
  UNKNOWN
}
