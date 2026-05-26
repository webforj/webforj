package com.webforj.sink.geolocation;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGeolocationEvent;
import com.basis.bbj.proxies.sysgui.BBjGeolocation;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.Geolocation;
import com.webforj.component.event.sink.DwcEventSink;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.event.geolocation.GeolocationPosition;
import com.webforj.event.geolocation.GeolocationStatus;
import com.webforj.event.geolocation.GeolocationWatchEvent;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * The {@code GeolocationWatchEventSink} implements the required logic for setting and removing the
 * callback on the underlying BBj geolocation for the watch event. It delegates the BBj event to the
 * corresponding event listener on the {@link Geolocation}.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class GeolocationWatchEventSink implements DwcEventSink<Geolocation> {

  private final Geolocation geolocation;
  private final EventDispatcher dispatcher;

  /**
   * Constructs a new instance of {@link GeolocationWatchEventSink}.
   *
   * @param geolocation the associated geolocation
   * @param dispatcher the event dispatcher
   */
  public GeolocationWatchEventSink(Geolocation geolocation, EventDispatcher dispatcher) {
    this.geolocation = geolocation;
    this.dispatcher = dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String setCallback(Object options) {
    if (isConnected()) {
      try {
        getBbjGeolocation().setCallback(SysGuiEventConstants.ON_GEOLOCATION_WATCH, this,
            "handleEvent");

        return String.valueOf(SysGuiEventConstants.ON_GEOLOCATION_WATCH);
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to set geolocation watch callback.", e);
      }
    }

    return null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeCallback(String id) {
    if (isConnected()) {
      try {
        getBbjGeolocation().clearCallback(SysGuiEventConstants.ON_GEOLOCATION_WATCH);
      } catch (BBjException e) {
        throw new WebforjRuntimeException("Failed to clear geolocation watch callback.", e);
      }
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final EventDispatcher getEventDispatcher() {
    return this.dispatcher;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final boolean isConnected() {
    return this.geolocation != null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isMultipleCallbacks() {
    return false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public Geolocation getComponent() {
    return this.geolocation;
  }

  /**
   * Handles the BBj event and dispatches a new {@link GeolocationWatchEvent}.
   *
   * @param bbjEvent the BBj event to handle
   */
  public void handleEvent(BBjEvent bbjEvent) {
    BBjGeolocationEvent geoEvent = (BBjGeolocationEvent) bbjEvent;
    GeolocationStatus status = GeolocationStatus.fromCode(geoEvent.getStatus());
    GeolocationPosition position =
        status == GeolocationStatus.SUCCESS ? toPosition(geoEvent) : null;

    dispatcher.dispatchEvent(
        new GeolocationWatchEvent(geolocation, status, position, geoEvent.getMessage()));
  }

  /**
   * Gets the BBj geolocation instance.
   *
   * @return the BBj geolocation instance
   */
  protected BBjGeolocation getBbjGeolocation() {
    try {
      return getEnvironment().getSysGui().getGeolocation();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to access geolocation subsystem.", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }

  private static GeolocationPosition toPosition(BBjGeolocationEvent event) {
    return new GeolocationPosition(event.getLatitude(), event.getLongitude(), event.getAccuracy(),
        event.getTimestamp(), event.getAltitude(), event.getAltitudeAccuracy(), event.getHeading(),
        event.getSpeed());
  }
}
