package com.webforj.bbj.security.store;

import com.webforj.Environment;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.LocalStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;

/**
 * {@link BBjRequestedLocationStore} that keeps the requested location in the browser local storage.
 *
 * <p>
 * Only paths inside the app are read back. Any other stored value is discarded. When the local
 * storage is not available the store stays empty. Failures are logged and never thrown.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjBrowserRequestedLocationStore implements BBjRequestedLocationStore {
  private static final Logger logger =
      System.getLogger(BBjBrowserRequestedLocationStore.class.getName());
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveLocation(Location location) {
    try {
      getStorage().ifPresent(it -> it.setItem(KEY, location.getFullURI()));
    } catch (Exception e) {
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
      var storage = getStorage();
      return storage.map(it -> it.get(KEY)).filter(it -> {
        if (!isRelativePath(it)) {
          clearLocation();
          logger.log(Level.WARNING,
              "Discarded a requested location that is not a path inside the app");
          return false;
        }
        return true;
      }).map(Location::new);
    } catch (Exception e) {
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
      getStorage().ifPresent(it -> it.remove(KEY));
    } catch (Exception e) {
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
  static boolean isRelativePath(String path) {
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

  private Optional<LocalStorage> getStorage() {
    return Optional.ofNullable(Environment.getCurrent() == null ? null : LocalStorage.getCurrent());
  }

}
