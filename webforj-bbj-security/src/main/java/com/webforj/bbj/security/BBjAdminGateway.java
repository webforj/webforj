package com.webforj.bbj.security;

/**
 * The calls of the library to the BBj Admin API.
 *
 * <p>
 * Every call blocks the calling thread and must not run on the UI thread. Every call throws a
 * {@link BBjAdminFailure} when the server refuses it or cannot be reached.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
interface BBjAdminGateway {

  /**
   * Logs a user in and returns the session with a new token.
   *
   * @param userName the BBj username
   * @param password the password of the user
   * @return the session
   *
   * @throws BBjAdminFailure when the login fails
   * @throws NullPointerException if an argument is {@code null}
   */
  BBjLoginSession openSession(String userName, String password);

  /**
   * Reads the username the server knows for the token of the session.
   *
   * @param session the session to check
   * @return the username
   *
   * @throws BBjAdminFailure when the token is refused or the server fails
   * @throws NullPointerException if the session is {@code null}
   */
  String readUserName(BBjLoginSession session);

  /**
   * Replaces the token of the session with a new one of the configured lifetime. The old token
   * stops working.
   *
   * @param session the session to update
   * @return the session with the new token and expiry
   *
   * @throws BBjAdminFailure when the token is refused or the server fails
   * @throws NullPointerException if the session is {@code null}
   */
  BBjLoginSession updateSession(BBjLoginSession session);

  /**
   * Ends the token of the session. The old token stops working.
   *
   * @param session the session to close
   *
   * @throws BBjAdminFailure when the token is refused or the server fails
   * @throws NullPointerException if the session is {@code null}
   */
  void closeSession(BBjLoginSession session);
}
