package com.webforj.spring;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Webforj boot configuration properties.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@ConfigurationProperties(prefix = "webforj")
public class WebforjConfigurationProperties {

  /**
   * The URL mapping for the Webforj servlet.
   *
   * <p>
   * This property defaults to "/*" if not specified.
   * </p>
   */
  private String servletMapping = "/*";

  /**
   * Sets the URL mapping for the Webforj servlet.
   *
   * @param servletMapping the URL mapping for the Webforj servlet
   */
  public void setServletMapping(String servletMapping) {
    this.servletMapping = servletMapping;
  }

  /**
   * Sets the URL mapping for the Webforj servlet.
   *
   * @return the URL mapping for the Webforj servlet
   */
  public String getServletMapping() {
    return servletMapping;
  }
}
