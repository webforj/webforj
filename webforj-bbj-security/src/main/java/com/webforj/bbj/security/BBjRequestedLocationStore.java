package com.webforj.bbj.security;

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
