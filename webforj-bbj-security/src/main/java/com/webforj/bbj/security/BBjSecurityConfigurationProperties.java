package com.webforj.bbj.security;

import com.typesafe.config.Config;

/**
 * BBjSecurityConfigurationProperties
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public class BBjSecurityConfigurationProperties {
  private Boolean enabled = true;
  private Boolean secureByDefault = true;
  private String authenticationPath;
  private String denyPath;

  public Boolean getEnabled() {
    return enabled;
  }

  public void setEnabled(Boolean enabled) {
    this.enabled = enabled;
  }

  public Boolean getSecureByDefault() {
    return secureByDefault;
  }

  public void setSecureByDefault(Boolean secureByDefault) {
    this.secureByDefault = secureByDefault;
  }

  public String getAuthenticationPath() {
    return authenticationPath;
  }

  public void setAuthenticationPath(String authenticationPath) {
    this.authenticationPath = authenticationPath;
  }

  public String getDenyPath() {
    return denyPath;
  }

  public void setDenyPath(String denyPath) {
    this.denyPath = denyPath;
  }

}
