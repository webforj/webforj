package com.webforj.bbj.security.store;

import com.webforj.Environment;
import com.webforj.router.history.Location;
import java.util.Optional;

/**
 * Stores the location a user requested before being sent to authenticate.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public interface BBjRequestedLocationStore {

  /**
   * Creates the requested location store for an environment.
   *
   * <p>
   * When the environment has an HTTP session, the location is kept in the HTTP session. Otherwise
   * it is kept in the local storage of the browser.
   * </p>
   *
   * @param environment the environment of the application
   * @return the requested location store
   */
  static BBjRequestedLocationStore create(Environment environment) {
    if (environment.getSessionAccessor().isPresent()) {
      return new BBjHttpSessionRequestedLocationStore();
    }

    return new BBjBrowserRequestedLocationStore();
  }

  /**
   * Saves the requested location and replaces a location saved earlier.
   *
   * @param location the requested location
   */
  void saveLocation(Location location);

  /**
   * Reads the saved location.
   *
   * @return the saved location, or empty if none is saved
   */
  Optional<Location> readLocation();

  /**
   * Removes the saved location.
   */
  void clearLocation();

}
