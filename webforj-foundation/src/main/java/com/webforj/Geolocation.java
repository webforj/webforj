package com.webforj;

import com.basis.bbj.proxies.sysgui.BBjGeolocation;
import com.basis.bbj.proxyif.SysGuiEventConstants;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.environment.ObjectTable;
import com.webforj.event.geolocation.GeolocationEventHandler;
import com.webforj.event.geolocation.GeolocationPosition;
import com.webforj.event.geolocation.GeolocationStatus;
import com.webforj.event.geolocation.GeolocationWatchEvent;
import com.webforj.exceptions.WebforjGeolocationException;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.sink.geolocation.GeolocationWatchEventSink;
import com.webforj.sink.geolocation.GeolocationWatchEventSinkRegistry;
import java.util.function.Consumer;

/**
 * Provides an interface to the browser's geolocation subsystem.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class Geolocation {

  private final EventDispatcher eventDispatcher = new EventDispatcher();
  private final GeolocationEventHandler eventHandler = new GeolocationEventHandler();
  private final GeolocationWatchEventSink watchSink =
      new GeolocationWatchEventSink(this, eventDispatcher);
  private final GeolocationWatchEventSinkRegistry watchRegistry =
      new GeolocationWatchEventSinkRegistry(watchSink);
  private boolean positionCallbackRegistered = false;

  private Geolocation() {}

  /**
   * Returns the geolocation instance for the current environment.
   *
   * @return the geolocation instance
   */
  public static Geolocation getCurrent() {
    String key = Geolocation.class.getName();
    if (ObjectTable.contains(key)) {
      return (Geolocation) ObjectTable.get(key);
    }

    Geolocation instance = new Geolocation();
    ObjectTable.put(key, instance);

    return instance;
  }

  /**
   * Returns whether a geolocation instance is available for the current environment.
   *
   * @return {@code true} when a geolocation instance is available
   */
  public static boolean isPresent() {
    return getCurrent() != null;
  }

  /**
   * Executes the given consumer with the geolocation instance when one is available.
   *
   * @param consumer the consumer to execute
   */
  public static void ifPresent(Consumer<Geolocation> consumer) {
    if (Geolocation.isPresent()) {
      consumer.accept(Geolocation.getCurrent());
    }
  }

  /**
   * Sets the high accuracy option for use in subsequent requests for geolocation information.
   *
   * <p>
   * The high accuracy attribute provides a hint that the application would like to receive the best
   * possible results. This may result in slower response times or increased power consumption. The
   * user might also deny this capability, or the device might not be able to provide more accurate
   * results than if the flag wasn't specified. The intended purpose of this attribute is to allow
   * applications to inform the browser that they do not require high accuracy geolocation fixes
   * and, therefore, the browser can avoid using geolocation providers that consume a significant
   * amount of power (e.g. GPS). This is especially useful for applications running on battery
   * powered devices, such as mobile phones.
   * </p>
   *
   * @param highAccuracy indicates whether the application would like to receive the most accurate
   *        results available
   * @return this geolocation instance
   */
  public Geolocation useHighAccuracy(boolean highAccuracy) {
    getBbjGeolocation().setHighAccuracy(highAccuracy);

    return this;
  }

  /**
   * Returns the high accuracy option that will be used in subsequent requests for geolocation
   * information.
   *
   * @return whether subsequent requests for geolocation information will request the most accurate
   *         results available
   * @see #useHighAccuracy(boolean)
   */
  public boolean isHighAccuracy() {
    return getBbjGeolocation().isHighAccuracy();
  }

  /**
   * Sets the timeout option for use in subsequent requests for geolocation information.
   *
   * <p>
   * The timeout attribute denotes the maximum length of time (expressed in seconds) that is allowed
   * to pass from the call to {@link #getCurrentPosition()} or a watch listener until the browser
   * returns a position. If the browser is unable to successfully acquire a new position before the
   * given timeout elapses, and no other errors have occurred in this interval, the request is
   * reported with the {@link GeolocationStatus#TIMEOUT} status. Note that the time that is spent
   * obtaining the user permission is not included in the period covered by the timeout attribute.
   * The timeout attribute only applies to the location acquisition operation.
   * </p>
   *
   * @param seconds the timeout value in seconds
   * @return this geolocation instance
   */
  public Geolocation useTimeout(double seconds) {
    getBbjGeolocation().setTimeout(BasisNumber.valueOf(seconds));

    return this;
  }

  /**
   * Returns the timeout value in seconds that will be used in subsequent requests for geolocation
   * information.
   *
   * @return the timeout value in seconds
   * @see #useTimeout(double)
   */
  public double getTimeout() {
    return getBbjGeolocation().getTimeout().doubleValue();
  }

  /**
   * Sets the maximum age option for use in subsequent requests for geolocation information.
   *
   * <p>
   * The maximum age attribute indicates that the application is willing to accept a cached position
   * whose age is no greater than the specified time in seconds. If maximum age is set to {@code 0},
   * the browser must immediately attempt to acquire a new position object. If the browser does not
   * have a cached position available whose age is no greater than the specified maximum age, then
   * it must acquire a new position object. In case of a watch, the maximum age refers to the first
   * position object returned by the browser.
   * </p>
   *
   * @param seconds the maximum age in seconds of any cached position ({@code 0} to acquire a
   *        current position)
   * @return this geolocation instance
   */
  public Geolocation useMaximumAge(double seconds) {
    getBbjGeolocation().setMaximumAge(BasisNumber.valueOf(seconds));

    return this;
  }

  /**
   * Returns the maximum age in seconds of any cached position that will be used in subsequent
   * requests for geolocation information.
   *
   * @return the maximum age in seconds of any cached position ({@code 0} to acquire a current
   *         position)
   * @see #useMaximumAge(double)
   */
  public double getMaximumAge() {
    return getBbjGeolocation().getMaximumAge().doubleValue();
  }

  /**
   * Requests that the browser pass back geolocation information in a subsequent watch event or
   * pending result.
   *
   * <p>
   * This method does not directly return geolocation information. It requests that the information
   * be returned through the returned {@link PendingResult}, which completes with the reported
   * {@link GeolocationPosition} or exceptionally with a {@link WebforjGeolocationException}
   * carrying the matching {@link GeolocationStatus} when the browser is unable to obtain a
   * position.
   * </p>
   *
   * @return a pending result that resolves with the device's position
   */
  public PendingResult<GeolocationPosition> getCurrentPosition() {
    PendingResult<GeolocationPosition> pending = new PendingResult<>();

    try {
      ensurePositionCallbackRegistered();
      eventHandler.enqueuePositionRequest(pending);
      getBbjGeolocation().getCurrentPosition();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to request current geolocation position.", e);
    }

    return pending;
  }

  /**
   * Adds a listener that is notified each time the browser reports a position update.
   *
   * <p>
   * The browser begins watching the position when the first listener is added and stops watching
   * once the last listener is removed.
   * </p>
   *
   * @param listener the listener to add
   * @return a registration that can be used to remove the listener
   */
  public ListenerRegistration<GeolocationWatchEvent> addWatchListener(
      EventListener<GeolocationWatchEvent> listener) {
    return watchRegistry.addEventListener(listener);
  }

  /**
   * Alias for {@link #addWatchListener(EventListener)}.
   *
   * @param listener the listener to add
   * @return a registration that can be used to remove the listener
   */
  public ListenerRegistration<GeolocationWatchEvent> onWatch(
      EventListener<GeolocationWatchEvent> listener) {
    return addWatchListener(listener);
  }

  private void ensurePositionCallbackRegistered() throws BBjException {
    if (positionCallbackRegistered) {
      return;
    }

    getBbjGeolocation().setCallback(SysGuiEventConstants.ON_GEOLOCATION_POSITION, eventHandler,
        "handlePositionEvent");
    positionCallbackRegistered = true;
  }

  GeolocationEventHandler getEventHandler() {
    return eventHandler;
  }

  GeolocationWatchEventSinkRegistry getWatchRegistry() {
    return watchRegistry;
  }

  GeolocationWatchEventSink getWatchSink() {
    return watchSink;
  }

  BBjGeolocation getBbjGeolocation() {
    try {
      return getEnvironment().getSysGui().getGeolocation();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to access geolocation subsystem.", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }

  void destroy() {
    eventDispatcher.removeAllListeners();
    ObjectTable.put(Geolocation.class.getName(), null);
  }
}
