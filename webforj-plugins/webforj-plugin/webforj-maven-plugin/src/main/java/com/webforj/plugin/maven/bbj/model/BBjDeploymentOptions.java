package com.webforj.plugin.maven.bbj.model;

import java.util.Objects;

/**
 * Configures the BBj deployment transport.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjDeploymentOptions {

  private String url = "";

  /**
   * Gets the base address of BBjShip.
   *
   * @return the base address of BBjShip
   */
  public String getUrl() {
    return url;
  }

  /**
   * Sets the base address of BBjShip.
   *
   * @param url the base address of BBjShip
   *
   * @throws NullPointerException if url is null
   */
  public void setUrl(String url) {
    this.url = Objects.requireNonNull(url);
  }
}
