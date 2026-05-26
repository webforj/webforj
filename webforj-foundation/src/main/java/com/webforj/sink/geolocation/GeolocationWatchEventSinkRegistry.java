package com.webforj.sink.geolocation;

import com.webforj.Geolocation;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.event.geolocation.GeolocationWatchEvent;

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
