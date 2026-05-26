package com.webforj.event.geolocation;

import java.util.Objects;
import java.util.Optional;

/**
 * The device's geographic position at a point in time.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class GeolocationPosition {

  private final double latitude;
  private final double longitude;
  private final double accuracy;
  private final long timestamp;
  private final Double altitude;
  private final Double altitudeAccuracy;
  private final Double heading;
  private final Double speed;

  /**
   * Creates a new position.
   *
   * @param latitude the latitude in decimal degrees
   * @param longitude the longitude in decimal degrees
   * @param accuracy the accuracy of the latitude and longitude coordinates, in meters
   * @param timestamp the time when this position information was acquired, in milliseconds since
   *        January 1st, 1970 UTC
   * @param altitude the height of the position above the WGS84 ellipsoid, in meters, or
   *        {@code null} when the device cannot provide altitude information
   * @param altitudeAccuracy the accuracy of the altitude, in meters, or {@code null} when the
   *        device cannot provide altitude information
   * @param heading the direction of travel of the device, in degrees, or {@code null} when the
   *        device cannot provide heading information
   * @param speed the current ground speed of the device, in meters per second, or {@code null} when
   *        the device cannot provide speed information
   */
  @SuppressWarnings("java:S107")
  public GeolocationPosition(double latitude, double longitude, double accuracy, long timestamp,
      Double altitude, Double altitudeAccuracy, Double heading, Double speed) {
    this.latitude = latitude;
    this.longitude = longitude;
    this.accuracy = accuracy;
    this.timestamp = timestamp;
    this.altitude = altitude;
    this.altitudeAccuracy = altitudeAccuracy;
    this.heading = heading;
    this.speed = speed;
  }

  /**
   * Returns the latitude in decimal degrees.
   *
   * <p>
   * The latitude attribute is a geographic coordinate specified in decimal degrees. This value is
   * only meaningful when the request was successful.
   * </p>
   *
   * @return the latitude in decimal degrees
   */
  public double getLatitude() {
    return latitude;
  }

  /**
   * Returns the longitude in decimal degrees.
   *
   * <p>
   * The longitude attribute is a geographic coordinate specified in decimal degrees. This value is
   * only meaningful when the request was successful.
   * </p>
   *
   * @return the longitude in decimal degrees
   */
  public double getLongitude() {
    return longitude;
  }

  /**
   * Returns the accuracy of the latitude and longitude coordinates.
   *
   * <p>
   * The accuracy attribute denotes the accuracy level of the latitude and longitude coordinates. It
   * is specified in meters and must be supported by all browsers. The value of the accuracy
   * attribute must be a non negative real number. This value is only meaningful when the request
   * was successful.
   * </p>
   *
   * @return the accuracy in meters
   */
  public double getAccuracy() {
    return accuracy;
  }

  /**
   * Returns the time when this position information was acquired.
   *
   * <p>
   * The timestamp is the number of milliseconds since January 1st, 1970 UTC. To convert to a
   * {@link java.util.Date} value, use {@code new java.util.Date(position.getTimestamp())}.
   * </p>
   *
   * @return the timestamp in milliseconds
   */
  public long getTimestamp() {
    return timestamp;
  }

  /**
   * Returns the altitude in meters. If the altitude is unavailable, the value is empty.
   *
   * <p>
   * The altitude attribute denotes the height of the position, specified in meters above the WGS84
   * ellipsoid. If the browser cannot provide altitude information, the value is empty. Multiply by
   * approximately {@code 3.28} to convert from meters to feet.
   * </p>
   *
   * @return the altitude in meters, or an empty {@link Optional} when the browser cannot provide
   *         altitude information
   */
  public Optional<Double> getAltitude() {
    return Optional.ofNullable(altitude);
  }

  /**
   * Returns the accuracy level of the altitude value, specified in meters.
   *
   * <p>
   * The altitudeAccuracy attribute is specified in meters. If the browser cannot provide altitude
   * information, the value is empty. Otherwise, the value of the altitudeAccuracy attribute must be
   * a non negative real number.
   * </p>
   *
   * @return the altitude accuracy in meters, or an empty {@link Optional} when the browser cannot
   *         provide altitude information
   */
  public Optional<Double> getAltitudeAccuracy() {
    return Optional.ofNullable(altitudeAccuracy);
  }

  /**
   * Returns the current heading in degrees.
   *
   * <p>
   * The heading attribute denotes the direction of travel of the hosting device and is specified in
   * degrees, where {@code 0 <= heading < 360}, counting clockwise relative to the true north. If
   * the browser cannot provide heading information, the value is empty. If the hosting device is
   * stationary (i.e. the value of the speed attribute is {@code 0}), then the value of the heading
   * attribute is {@link Double#NaN}.
   * </p>
   *
   * @return the heading in degrees, or an empty {@link Optional} when heading information is
   *         unavailable
   */
  public Optional<Double> getHeading() {
    return Optional.ofNullable(heading);
  }

  /**
   * Returns the speed in meters per second.
   *
   * <p>
   * The speed attribute denotes the current ground speed of the hosting device and is specified in
   * meters per second. If the browser cannot provide speed information, the value is empty.
   * Otherwise, the value of the speed attribute must be a non negative real number. Multiply by
   * {@code 3.6} to convert to km/h. Multiply by approximately {@code 2.237} to convert to mph. For
   * example, a reported speed value of 25 m/s corresponds to 90 km/h, or about 56 mph.
   * </p>
   *
   * @return the speed in meters per second, or an empty {@link Optional} when the browser cannot
   *         provide speed information
   */
  public Optional<Double> getSpeed() {
    return Optional.ofNullable(speed);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }

    if (!(obj instanceof GeolocationPosition)) {
      return false;
    }

    GeolocationPosition other = (GeolocationPosition) obj;

    return Double.compare(latitude, other.latitude) == 0
        && Double.compare(longitude, other.longitude) == 0
        && Double.compare(accuracy, other.accuracy) == 0 && timestamp == other.timestamp
        && Objects.equals(altitude, other.altitude)
        && Objects.equals(altitudeAccuracy, other.altitudeAccuracy)
        && Objects.equals(heading, other.heading) && Objects.equals(speed, other.speed);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int hashCode() {
    return Objects.hash(latitude, longitude, accuracy, timestamp, altitude, altitudeAccuracy,
        heading, speed);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String toString() {
    return "GeolocationPosition[latitude=" + latitude + ", longitude=" + longitude + ", accuracy="
        + accuracy + ", timestamp=" + timestamp + ", altitude=" + altitude + ", altitudeAccuracy="
        + altitudeAccuracy + ", heading=" + heading + ", speed=" + speed + "]";
  }
}
