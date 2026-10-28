package com.webforj.bbj.security.store;

import com.webforj.environment.SessionObjectTable;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

/**
 * {@link BBjRequestedLocationStore} that keeps the requested location in the HTTP session.
 *
 * <p>
 * Only paths inside the app are read back. Any other stored value is discarded. When the HTTP
 * session is not available the store stays empty. Failures are logged and never thrown.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjHttpSessionRequestedLocationStore implements BBjRequestedLocationStore {
  private static final Logger logger =
      System.getLogger(BBjHttpSessionRequestedLocationStore.class.getName());
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveLocation(Location location) {
    try {
      SessionObjectTable.put(KEY, location.getFullURI());
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
      if (!SessionObjectTable.contains(KEY)) {
        return Optional.empty();
      }

      if (SessionObjectTable.get(KEY) instanceof String path
          && BBjBrowserRequestedLocationStore.isRelativePath(path)) {
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
      SessionObjectTable.clear(KEY);
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, "The requested location could not be cleared ({0})",
          e.getClass().getName());
    }
  }

}
