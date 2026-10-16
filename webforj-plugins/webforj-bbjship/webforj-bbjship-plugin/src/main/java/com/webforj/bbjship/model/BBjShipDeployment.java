package com.webforj.bbjship.model;

/**
 * Holds the answer that names a deployed application and its address.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@SuppressWarnings("java:S6206")
public final class BBjShipDeployment {

  private final String name;
  private final String url;

  /**
   * Creates a deployment answer.
   *
   * @param name the publish name
   * @param url the path of the application on the BBj server
   */
  public BBjShipDeployment(String name, String url) {
    this.name = name;
    this.url = url;
  }

  /**
   * Gets the publish name.
   *
   * @return the publish name
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the path of the application on the BBj server.
   *
   * @return the application path
   */
  public String getUrl() {
    return url;
  }
}
