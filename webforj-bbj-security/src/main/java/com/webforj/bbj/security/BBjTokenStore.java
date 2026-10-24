package com.webforj.bbj.security;

import java.util.Optional;

/**
 * Keeps the login session of one visitor outside the app instance, so that a reload of the page can
 * find it again.
 *
 * <p>
 * Every method runs on the Environment thread only. {@link #readSession()} and
 * {@link #clearSession()} never throw. A stored value that cannot be used is removed.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
interface BBjTokenStore {

  /**
   * Keeps the session, replacing the kept one.
   *
   * @param session the session to keep
   *
   * @throws IllegalStateException if the session could not be kept, the message never holds a
   *         secret and there is no cause
   */
  void saveSession(BBjLoginSession session);

  /**
   * Reads the kept session.
   *
   * @return the kept session, or empty when none is kept or the kept value is expired or not usable
   */
  Optional<BBjLoginSession> readSession();

  /**
   * Forgets the kept session.
   */
  void clearSession();
}
