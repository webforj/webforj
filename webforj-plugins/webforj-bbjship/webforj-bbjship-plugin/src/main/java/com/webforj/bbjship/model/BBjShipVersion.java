package com.webforj.bbjship.model;

import java.util.List;

/**
 * Holds the answer that names the version of BBjShip and the Java arguments BBjServices runs with.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class BBjShipVersion {

  private final String version;
  private final List<String> javaArguments;

  /**
   * Creates a version answer.
   *
   * @param version the BBjShip version
   * @param javaArguments the Java arguments BBjServices runs with
   */
  public BBjShipVersion(String version, List<String> javaArguments) {
    this.version = version;
    this.javaArguments = List.copyOf(javaArguments);
  }

  /**
   * Gets the BBjShip version.
   *
   * @return the BBjShip version
   */
  public String getVersion() {
    return version;
  }

  /**
   * Gets the Java arguments BBjServices runs with.
   *
   * @return the unmodifiable arguments
   */
  public List<String> getJavaArguments() {
    return javaArguments;
  }
}
