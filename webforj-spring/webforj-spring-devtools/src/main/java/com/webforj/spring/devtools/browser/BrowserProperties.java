package com.webforj.spring.devtools.browser;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Configuration properties for browser launcher.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@ConfigurationProperties(prefix = "webforj.devtools.browser")
public class BrowserProperties {

  /**
   * Enable automatic browser opening when the application starts.
   */
  private boolean open = false;

  /**
   * Host type to use in the browser URL. Supported values are: localhost, hostname, ip-address
   */
  private HostType host = HostType.LOCALHOST;

  public enum HostType {
    LOCALHOST, HOSTNAME, IP_ADDRESS
  }

  /**
   * Returns whether automatic browser opening is enabled.
   *
   * @return true if browser opening is enabled, false otherwise
   */
  public boolean isOpen() {
    return open;
  }

  /**
   * Sets whether automatic browser opening is enabled.
   *
   * @param open true to enable browser opening, false to disable
   */
  public void setOpen(boolean open) {
    this.open = open;
  }

  /**
   * Returns the host type to use in the browser URL.
   *
   * @return the host type
   */
  public HostType getHost() {
    return host;
  }

  /**
   * Sets the host type to use in the browser URL.
   *
   * @param host the host type
   */
  public void setHost(HostType host) {
    this.host = host;
  }
}
