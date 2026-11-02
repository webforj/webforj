package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjSession;
import com.webforj.environment.SessionObjectTable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.Optional;

/**
 * Keeps the {@link BBjSession} of the signed in user in the HTTP session.
 *
 * <p>
 * All tabs of one HTTP session share the saved session, so it survives a page reload. Nothing
 * secret is logged. A failure is logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjSecurityStore {
  private static final Logger logger = System.getLogger(BBjSecurityStore.class.getName());
  private static final String SESSION_KEY = "webforj-bbj-session";
  private static final String NOT_KEPT = "The sign in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private final Clock clock;

  BBjSecurityStore(Clock clock) {
    this.clock = clock;
  }

  BBjSecurityStore() {
    this(Clock.systemUTC());
  }

  /**
   * Saves the session shared by all tabs of the HTTP session and replaces the session saved before.
   *
   * @param session the session to keep
   *
   * @throws IllegalStateException if the session cannot be kept
   */
  public void saveSession(BBjSession session) {
    try {
      SessionObjectTable.put(SESSION_KEY, session);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the HTTP session",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }
  }

  /**
   * Reads the session saved in the HTTP session.
   *
   * <p>
   * A session that is expired, or a stored value that is not a session, is removed and reported as
   * empty.
   * </p>
   *
   * @return the session, or empty when the HTTP session holds none that can be used
   */
  public Optional<BBjSession> readSession() {
    try {
      if (!SessionObjectTable.contains(SESSION_KEY)) {
        return Optional.empty();
      }

      if (SessionObjectTable.get(SESSION_KEY) instanceof BBjSession session
          && session.getExpiresAt().isAfter(clock.instant())) {
        return Optional.of(session);
      }

      logger.log(Level.DEBUG, "A stored sign in is not usable and was removed");
      clearSession();

      return Optional.empty();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The stored sign in could not be read",
          e.getClass().getName());
      return Optional.empty();
    }
  }

  /**
   * Removes the session saved in the HTTP session. This method never throws.
   */
  public void clearSession() {
    try {
      SessionObjectTable.clear(SESSION_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be removed from the HTTP session",
          e.getClass().getName());
    }
  }
}
