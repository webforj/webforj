package com.webforj.bbj.security;

/**
 * Route security configuration properties of a BBj application.
 *
 * <p>
 * Security is enabled and routes are secure by default unless set otherwise. No
 * authentication path
 * and no deny path are set by default.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public class BBjSecurityConfigurationProperties {
  private Boolean enabled = true;
  private Boolean secureByDefault = true;
  private String authenticationPath;
  private String denyPath;
  private String adminHost = "127.0.0.1";
  private Integer adminPort = 2002;
  private Integer adminTimeoutSeconds = 10;
  private Integer tokenMinutes = 30;
  private String defaultSuccessPath = "/";
  private Boolean returnAfterLogin = true;

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
   * @return the authentication path, or null if none is set
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
   * @return the deny path, or null if none is set
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

  public String getAdminHost() {
    return adminHost;
  }

  public void setAdminHost(String adminHost) {
    this.adminHost = adminHost;
  }

  public Integer getAdminPort() {
    return adminPort;
  }

  public void setAdminPort(Integer adminPort) {
    this.adminPort = adminPort;
  }

  public Integer getAdminTimeoutSeconds() {
    return adminTimeoutSeconds;
  }

  public void setAdminTimeoutSeconds(Integer adminTimeoutSeconds) {
    this.adminTimeoutSeconds = adminTimeoutSeconds;
  }

  public Integer getTokenMinutes() {
    return tokenMinutes;
  }

  public void setTokenMinutes(Integer tokenMinutes) {
    this.tokenMinutes = tokenMinutes;
  }

  public String getDefaultSuccessPath() {
    return defaultSuccessPath;
  }

  public void setDefaultSuccessPath(String defaultSuccessPath) {
    this.defaultSuccessPath = defaultSuccessPath;
  }

  public Boolean getReturnAfterLogin() {
    return returnAfterLogin;
  }

  public void setReturnAfterLogin(Boolean returnAfterLogin) {
    this.returnAfterLogin = returnAfterLogin;
  }

}
