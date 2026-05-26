package com.webforj.event.geolocation;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.event.BBjGeolocationEvent;
import com.webforj.PendingResult;
import com.webforj.exceptions.WebforjGeolocationException;
import java.lang.System.Logger;
import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Handler for the {@code ON_GEOLOCATION_POSITION} BBj callback. Correlates incoming callbacks FIFO
 * with pending one shot requests registered.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class GeolocationEventHandler {

  private static final Logger logger = System.getLogger(GeolocationEventHandler.class.getName());
  private final Deque<PendingResult<GeolocationPosition>> pendingPositions = new ArrayDeque<>();

  /**
   * Constructs a new handler.
   */
  public GeolocationEventHandler() {
    // no-op
  }

  /**
   * Registers a pending one shot position request that will be completed on the next
   * {@code ON_GEOLOCATION_POSITION} callback.
   *
   * @param pending the pending result to complete
   */
  public void enqueuePositionRequest(PendingResult<GeolocationPosition> pending) {
    pendingPositions.add(pending);
  }

  /**
   * Handles a {@code ON_GEOLOCATION_POSITION} callback and completes the next pending request.
   *
   * @param event the BBj event
   */
  public void handlePositionEvent(BBjEvent event) {
    BBjGeolocationEvent geoEvent = (BBjGeolocationEvent) event;
    PendingResult<GeolocationPosition> pending = pendingPositions.poll();
    if (pending == null) {
      logger.log(Logger.Level.WARNING,
          "Received geolocation position event with no pending request to complete.");

      return;
    }

    GeolocationStatus status = GeolocationStatus.fromCode(geoEvent.getStatus());
    if (status == GeolocationStatus.SUCCESS) {
      pending.complete(toPosition(geoEvent));
    } else {
      pending.completeExceptionally(new WebforjGeolocationException(status, geoEvent.getMessage()));
    }
  }

  private static GeolocationPosition toPosition(BBjGeolocationEvent event) {
    return new GeolocationPosition(event.getLatitude(), event.getLongitude(), event.getAccuracy(),
        event.getTimestamp(), event.getAltitude(), event.getAltitudeAccuracy(), event.getHeading(),
        event.getSpeed());
  }
}
