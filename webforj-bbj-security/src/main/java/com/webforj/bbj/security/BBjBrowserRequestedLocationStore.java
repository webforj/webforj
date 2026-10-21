package com.webforj.bbj.security;

import com.webforj.router.history.Location;
import com.webforj.router.security.AbstractRouteSecurityManager;
import com.webforj.webstorage.SessionStorage;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * Keeps the requested location in the session storage of the browser tab.
 *
 * <p>
 * The location is kept as its full URI. A kept value that is not a path inside the app is discarded
 * when it is read, and one warning without the value is logged. Without a session storage nothing
 * is kept and nothing is read.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjBrowserRequestedLocationStore implements BBjRequestedLocationStore {
  private static final Logger logger =
      System.getLogger(BBjBrowserRequestedLocationStore.class.getName());
  private static final String KEY = AbstractRouteSecurityManager.PRE_AUTH_LOCATION_KEY;

  private final Supplier<SessionStorage> storageSupplier;

  BBjBrowserRequestedLocationStore(Supplier<SessionStorage> storageSupplier) {
    this.storageSupplier = storageSupplier;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void saveLocation(Location location) {
    try {
      findStorage().ifPresent(storage -> storage.setItem(KEY, location.getFullURI()));
    } catch (RuntimeException e) {
      logger.log(Level.WARNING, e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Optional<Location> readLocation() {
    Optional<SessionStorage> storage = findStorage();
    return storage.map(it -> it.getItem(KEY)).filter(it -> {
      if (!BBjRouteSecurityConfiguration.isRelativePath(it)) {
        clearLocation();
        logger.log(Level.WARNING,
            "Discarded a requested location that is not a path inside the app");
        return false;
      }
      return true;
    }).map(Location::new);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearLocation() {
    findStorage().ifPresent(storage -> storage.removeItem(KEY));
  }

  private Optional<SessionStorage> findStorage() {
    return Optional.ofNullable(storageSupplier.get());
  }
}
