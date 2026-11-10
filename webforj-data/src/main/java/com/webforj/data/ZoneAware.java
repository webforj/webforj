package com.webforj.data;

import java.time.ZoneId;

/**
 * Interface for classes that are aware of a time zone.
 *
 * @param <T> the type of the implementing class for fluent API support.
 *
 * @since 26.10
 * @author Hyyan Abo Fakher
 */
public interface ZoneAware<T> {

  /**
   * Sets the time zone.
   *
   * @param zone the time zone.
   *
   * @return the implementing instance.
   */
  T setZone(ZoneId zone);

  /**
   * Gets the time zone.
   *
   * @return the time zone.
   */
  ZoneId getZone();
}
