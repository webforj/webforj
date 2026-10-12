package com.webforj.bbjship.model;

import java.util.List;

/**
 * Holds the answer that names the version of BBjShip, the Java arguments BBjServices runs with and
 * the process id of BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class BBjShipVersion {

  private final String version;
  private final List<String> javaArguments;
  private final long pid;

  /**
   * Creates a version answer.
   *
   * @param version the BBjShip version
   * @param javaArguments the Java arguments BBjServices runs with
   * @param pid the process id of BBjServices
   */
  public BBjShipVersion(String version, List<String> javaArguments, long pid) {
    this.version = version;
    this.javaArguments = List.copyOf(javaArguments);
    this.pid = pid;
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

  /**
   * Gets the process id of BBjServices.
   *
   * @return the process id
   */
  public long getPid() {
    return pid;
  }
}
