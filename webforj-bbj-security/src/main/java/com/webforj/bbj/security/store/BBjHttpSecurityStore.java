package com.webforj.bbj.security.store;

import com.webforj.bbj.security.model.BBjSession;
import com.webforj.environment.SessionObjectTable;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.Optional;

/**
 * {@link BBjSecurityStore} that keeps the session and the requested location in the HTTP session.
 *
 * <p>
 * All tabs of one HTTP session share the saved session and the requested location. Nothing secret
 * is logged. A failure is logged by its class name only.
 * </p>
 *
 * <p>
 * Only paths inside the app are read back as the requested location. Any other stored value is
 * discarded. When the HTTP session is not available no location is kept. Failures of the location
 * are logged and never thrown.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjHttpSecurityStore implements BBjSecurityStore {
  private static final Logger logger = System.getLogger(BBjHttpSecurityStore.class.getName());
  private static final String SESSION_KEY = "webforj-bbj-session";
  private static final String LOCATION_KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;
  private static final String NOT_KEPT = "The sign in could not be kept";
  private static final String FAILURE = "{0} ({1})";
  private final Clock clock;

  BBjHttpSecurityStore(Clock clock) {
    this.clock = clock;
  }

  BBjHttpSecurityStore() {
    this(Clock.systemUTC());
  }

  /**
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
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
   * {@inheritDoc}
   */
  @Override
  public void clearSession() {
    try {
      SessionObjectTable.clear(SESSION_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, FAILURE, "The sign in could not be removed from the HTTP session",
          e.getClass().getName());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveLocation(Location location) {
    try {
      SessionObjectTable.put(LOCATION_KEY, location.getFullURI());
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The requested location could not be saved ({0})",
          e.getClass().getName());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> readLocation() {
    try {
      if (!SessionObjectTable.contains(LOCATION_KEY)) {
        return Optional.empty();
      }

      if (SessionObjectTable.get(LOCATION_KEY) instanceof String path
          && BBjBrowserSecurityStore.isRelativePath(path)) {
        return Optional.of(new Location(path));
      }

      clearLocation();
      logger.log(Level.WARNING, "Discarded a requested location that is not a path inside the app");

      return Optional.empty();
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The requested location could not be read ({0})",
          e.getClass().getName());
      return Optional.empty();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearLocation() {
    try {
      SessionObjectTable.clear(LOCATION_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The requested location could not be cleared ({0})",
          e.getClass().getName());
    }
  }

}
