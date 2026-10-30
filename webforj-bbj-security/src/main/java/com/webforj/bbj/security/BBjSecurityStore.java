package com.webforj.bbj.security;

import com.webforj.bbj.security.model.BBjSession;
import com.webforj.environment.SessionObjectTable;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.time.Clock;
import java.util.Optional;

/**
 * Keeps the {@link BBjSession} of the signed in user in the HTTP session and reads the location the
 * user requested before being sent to authenticate.
 *
 * <p>
 * All tabs of one HTTP session share the saved session and the requested location, so both survive
 * a page reload. Nothing secret is logged. A failure is logged by its class name only.
 * </p>
 *
 * <p>
 * Only paths inside the app are read back as the requested location. Any other stored value is
 * discarded. When the HTTP session is not available no session is kept and no location is read.
 * Failures of the location are logged and never thrown.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjSecurityStore {
  private static final Logger logger = System.getLogger(BBjSecurityStore.class.getName());
  private static final String SESSION_KEY = "webforj-bbj-session";
  private static final String LOCATION_KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;
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

  /**
   * Reads the requested location saved in the HTTP session.
   *
   * @return the saved location, or empty if none is saved
   */
  public Optional<Location> readLocation() {
    try {
      if (!SessionObjectTable.contains(LOCATION_KEY)) {
        return Optional.empty();
      }

      if (SessionObjectTable.get(LOCATION_KEY) instanceof String path && isRelativePath(path)) {
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
   * Removes the requested location saved in the HTTP session.
   */
  public void clearLocation() {
    try {
      SessionObjectTable.clear(LOCATION_KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The requested location could not be cleared ({0})",
          e.getClass().getName());
    }
  }

  /**
   * Tells whether a stored value is a path inside the app.
   *
   * <p>
   * A path inside the app starts with one slash, holds no backslash and is a valid location.
   * </p>
   *
   * @param path the stored value
   * @return true when the value is a path inside the app
   */
  private static boolean isRelativePath(String path) {
    if (!path.startsWith("/") || path.startsWith("//") || path.indexOf('\\') >= 0) {
      return false;
    }

    try {
      new Location(path);
      return true;
    } catch (IllegalArgumentException e) {
      return false;
    }
  }
}
