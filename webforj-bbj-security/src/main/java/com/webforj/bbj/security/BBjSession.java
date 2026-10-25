package com.webforj.bbj.security;

import java.time.Instant;

import com.webforj.bbj.security.model.BBjPrincipal;

/**
 * BBjSession
 */
public class BBjSession {
  private final BBjPrincipal principal;
  private final String token;
  private final Instant expiresAt;

  public BBjSession(BBjPrincipal principal, String token, Instant expiresAt) {
    this.principal = principal;
    this.token = token;
    this.expiresAt = expiresAt;
  }

  public BBjPrincipal getPrincipal() {
    return principal;
  }

  public String getToken() {
    return token;
  }

  public Instant getExpiresAt() {
    return expiresAt;
  }

  @Override
  public String toString() {
    return "BBjSession [principal=" + principal + ", expiresAt=" + expiresAt + "]";
  }

}
