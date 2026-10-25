package com.webforj.bbj.security.model;

/**
 * The outcome of a sign in.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public enum BBjLoginStatus {
  /** The user signed in. */
  AUTHENTICATED,

  /** The credentials were refused or are not acceptable. */
  INVALID_CREDENTIALS,

  /** The sign in could not be completed, for example because BBj cannot be reached. */
  UNAVAILABLE
}
