package com.webforj.spring.security;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Spring Security configuration properties for webforJ applications.
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@ConfigurationProperties(prefix = "webforj.security")
public class SpringSecurityConfigurationProperties {

  /**
   * Whether security is enabled for the application. When disabled, all security checks are
   * bypassed.
   */
  private Boolean enabled = true;

  /**
   * Whether routes are secure by default. When true, routes require authentication unless
   * explicitly marked with {@code @AnonymousAccess} or {@code @PermitAll}. When false, routes are
   * open by default and must be explicitly secured.
   */
  private Boolean secureByDefault = true;

  /**
   * The path to redirect users to when authentication is required. This is typically the login page
   * path.
   */
  private String authenticationPath;

  /**
   * The path to redirect users to when access is denied. This is shown when an authenticated user
   * tries to access a resource they're not authorized for, or for any other access denial scenario.
   */
  private String denyPath;

  /**
   * Gets whether security is enabled.
   *
   * @return true if security is enabled, false otherwise
   */
  public Boolean getEnabled() {
    return enabled;
  }

  /**
   * Sets whether security is enabled.
   *
   * @param enabled true to enable security, false to disable
   */
  public void setEnabled(Boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Gets whether routes are secure by default.
   *
   * @return true if routes are secure by default, false otherwise
   */
  public Boolean getSecureByDefault() {
    return secureByDefault;
  }

  /**
   * Sets whether routes are secure by default.
   *
   * @param secureByDefault true to make routes secure by default, false otherwise
   */
  public void setSecureByDefault(Boolean secureByDefault) {
    this.secureByDefault = secureByDefault;
  }

  /**
   * Gets the authentication path.
   *
   * @return the authentication path
   */
  public String getAuthenticationPath() {
    return authenticationPath;
  }

  /**
   * Sets the authentication path.
   *
   * @param authenticationPath the authentication path
   */
  public void setAuthenticationPath(String authenticationPath) {
    this.authenticationPath = authenticationPath;
  }

  /**
   * Gets the deny path.
   *
   * @return the deny path
   */
  public String getDenyPath() {
    return denyPath;
  }

  /**
   * Sets the deny path.
   *
   * @param denyPath the deny path
   */
  public void setDenyPath(String denyPath) {
    this.denyPath = denyPath;
  }
}
