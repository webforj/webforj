package com.webforj.bbj.security;

import com.webforj.router.history.Location;
import java.util.Optional;

/**
 * Keeps the location a visitor asked for until the visitor has signed in.
 *
 * <p>
 * Every method is synchronous and never throws for a missing or unusable value. An implementation
 * keeps the location where its visitor owns it, so that one visitor never reads the location of
 * another.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
interface BBjRequestedLocationStore {

  /**
   * Keeps the location, replacing a kept one.
   *
   * @param location the location the visitor asked for
   */
  void saveLocation(Location location);

  /**
   * Reads the kept location.
   *
   * @return the kept location, or empty when none is kept or the kept value is not usable
   */
  Optional<Location> readLocation();

  /**
   * Forgets the kept location.
   */
  void clearLocation();
}
