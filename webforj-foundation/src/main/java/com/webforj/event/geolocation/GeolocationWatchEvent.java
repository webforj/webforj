package com.webforj.event.geolocation;

import com.webforj.Geolocation;
import java.util.EventObject;
import java.util.Optional;

/**
 * An event fired whenever the browser reports a watched position update for a {@link Geolocation}.
 *
 * <p>
 * On a successful update the event carries a {@link GeolocationPosition}. On a failed update the
 * event carries the reported {@link GeolocationStatus} and an optional message that contains
 * additional information about the failure.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class GeolocationWatchEvent extends EventObject {

  private final GeolocationStatus status;
  private final transient GeolocationPosition position;
  private final String message;

  /**
   * Constructs a new watch event.
   *
   * @param source the geolocation that produced the event
   * @param status the status reported by the browser
   * @param position the reported position, or {@code null} when the status is not
   *        {@link GeolocationStatus#SUCCESS}
   * @param message additional information about the failure, or {@code null} on success
   */
  public GeolocationWatchEvent(Geolocation source, GeolocationStatus status,
      GeolocationPosition position, String message) {
    super(source);
    this.status = status;
    this.position = position;
    this.message = message;
  }

  /**
   * Returns the geolocation that produced the event.
   *
   * @return the geolocation instance
   */
  public Geolocation getGeolocation() {
    return (Geolocation) getSource();
  }

  /**
   * Returns the status reported by the browser.
   *
   * @return the status
   */
  public GeolocationStatus getStatus() {
    return status;
  }

  /**
   * Returns whether the event represents a successful position update.
   *
   * @return {@code true} when the status is {@link GeolocationStatus#SUCCESS}
   */
  public boolean isSuccess() {
    return status == GeolocationStatus.SUCCESS;
  }

  /**
   * Returns the position carried by the event when the update was successful.
   *
   * @return the position, or an empty {@link Optional} when the update failed
   */
  public Optional<GeolocationPosition> getPosition() {
    return Optional.ofNullable(position);
  }

  /**
   * May return additional information when the status indicates an error result.
   *
   * @return an optional error message only meaningful when the status is not
   *         {@link GeolocationStatus#SUCCESS}, or an empty {@link Optional} when the update
   *         succeeded or the browser did not provide a message
   */
  public Optional<String> getMessage() {
    return Optional.ofNullable(message);
  }
}
