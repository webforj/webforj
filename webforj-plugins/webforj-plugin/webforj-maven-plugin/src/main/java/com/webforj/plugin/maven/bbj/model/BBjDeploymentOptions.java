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
  private boolean allowUnencrypted;

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

  /**
   * Checks whether the administration credentials may travel over an unencrypted connection to
   * another machine.
   *
   * @return true when an unencrypted connection to another machine is allowed
   */
  public boolean isAllowUnencrypted() {
    return allowUnencrypted;
  }

  /**
   * Sets whether the administration credentials may travel over an unencrypted connection to
   * another machine.
   *
   * @param allowUnencrypted true to allow an unencrypted connection to another machine
   */
  public void setAllowUnencrypted(boolean allowUnencrypted) {
    this.allowUnencrypted = allowUnencrypted;
  }
}
