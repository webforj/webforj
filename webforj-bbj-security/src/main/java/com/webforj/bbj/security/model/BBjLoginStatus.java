package com.webforj.bbj.security.model;

/**
 * The outcome of a sign-in attempt.
 *
 * <p>
 * The enum may gain constants in later releases, so a caller keeps a default branch.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public enum BBjLoginStatus {
  /**
   * The visitor is signed in and the app navigated to the requested location.
   */
  AUTHENTICATED,

  /**
   * The username or the password was refused or left blank.
   */
  INVALID_CREDENTIALS,

  /**
   * The sign-in could not be completed right now, for example because the server could not be
   * reached in time or the account needs a sign-in step that is not supported yet.
   */
  UNAVAILABLE
}
