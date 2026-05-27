package com.webforj.geolocation.sink;

import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.geolocation.Geolocation;
import com.webforj.geolocation.event.GeolocationWatchEvent;

/**
 * {@code GeolocationWatchEventSinkRegistry} manages the event listeners for a geolocation watch
 * sink and the corresponding {@link GeolocationWatchEvent}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class GeolocationWatchEventSinkRegistry
    extends EventSinkListenerRegistry<GeolocationWatchEvent, Geolocation> {

  /**
   * Creates a new GeolocationWatchEventSinkRegistry.
   *
   * @param sink the corresponding sink to the event
   */
  public GeolocationWatchEventSinkRegistry(GeolocationWatchEventSink sink) {
    super(sink, GeolocationWatchEvent.class);
  }
}
