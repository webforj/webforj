package com.webforj.bbjship.foundation.model;

/**
 * Holds the deployment answer of BBjShip.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentResponse {

  private String name;
  private String url;

  /**
   * Gets the publish name of the deployed application.
   *
   * @return the publish name, null when the answer carries none
   */
  public String getName() {
    return name;
  }

  /**
   * Gets the path of the deployed application on the BBj server.
   *
   * @return the application path, null when the answer carries none
   */
  public String getUrl() {
    return url;
  }
}
