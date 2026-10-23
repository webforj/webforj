package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;
import java.util.function.Supplier;

import com.basis.bbj.processor.instruction.fa;
import com.webforj.Environment;
import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.SessionStorage;

/**
 * BBjBrowserRequestedLocationStore
 */
public final class BBjBrowserRequestedLocationStore implements BBjRequestedLocationStore {
  private static final Logger logger = System.getLogger(BBjBrowserRequestedLocationStore.class.getName());
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  @Override
  public void saveLocation(Location location) {
    try {
      getStorage().ifPresent(it -> it.setItem(KEY, location.getFullURI()));
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested location could not be saved ({0})", e.getClass().getName());
    }
  }

  @Override
  public Optional<Location> readLocation() {
    try {
      var storage = getStorage();
      return storage.map(it -> it.get(KEY))
          .filter(it -> {
            if (!isRelativePath(it)) {
              clearLocation();
              logger.log(Level.WARNING, "Discarded a requested location that is not a path inside the app");
              return false;
            }
            return true;
          }).map(Location::new);
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested location could not be read ({0})", e.getClass().getName());
      return Optional.empty();
    }
  }

  private boolean isRelativePath(String path) {
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

  @Override
  public void clearLocation() {
    try {
      getStorage().ifPresent(it -> it.remove(KEY));
    } catch (Exception e) {
      logger.log(Level.WARNING, "The requested locaiton could not be cleared ({0})", e.getClass().getName());
    }
  }

  private Optional<SessionStorage> getStorage() {
    return Optional.ofNullable(Environment.getCurrent() == null ? null : SessionStorage.getCurrent());
  }

}
