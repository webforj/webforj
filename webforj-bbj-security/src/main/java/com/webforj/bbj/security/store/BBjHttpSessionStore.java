package com.webforj.bbj.security.store;

import com.webforj.bbj.security.model.BBjSession;
import com.webforj.environment.SessionObjectTable;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.Optional;

/**
 * {@link BBjSessionStore} that keeps the session in the HTTP session.
 *
 * <p>
 * All tabs of one HTTP session share the saved session. Nothing secret is logged. A failure is
 * logged by its class name only.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjHttpSessionStore implements BBjSessionStore {
  private static final Logger logger = System.getLogger(BBjHttpSessionStore.class.getName());
  private static final String KEY = "webforj-bbj-session";
  private static final String NOT_KEPT = "The sign in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private final Clock clock;

  BBjHttpSessionStore(Clock clock) {
    this.clock = clock;
  }

  BBjHttpSessionStore() {
    this(Clock.systemUTC());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveSession(BBjSession session) {
    try {
      SessionObjectTable.put(KEY, session);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be written to the HTTP session",
          e.getClass().getName());
      throw new IllegalStateException(NOT_KEPT);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<BBjSession> readSession() {
    try {
      if (!SessionObjectTable.contains(KEY)) {
        return Optional.empty();
      }

      if (SessionObjectTable.get(KEY) instanceof BBjSession session
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
   * {@inheritDoc}
   */
  @Override
  public void clearSession() {
    try {
      SessionObjectTable.clear(KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be removed from the HTTP session",
          e.getClass().getName());
    }
  }

}
