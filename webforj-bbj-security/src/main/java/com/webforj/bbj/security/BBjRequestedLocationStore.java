package com.webforj.bbj.security;

import java.util.Optional;

import com.webforj.router.history.Location;

/**
 * BBjRequestLocationStore
 */
public interface BBjRequestedLocationStore {

  void saveLocation(Location location);

  Optional<Location> readLocation();

  void clearLocation();

}
