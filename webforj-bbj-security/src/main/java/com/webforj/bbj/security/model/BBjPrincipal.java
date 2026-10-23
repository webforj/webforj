package com.webforj.bbj.security.model;

import java.util.Set;

/**
 * Identity of a user authenticated through BBj.
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public class BBjPrincipal {
  private final String userName;
  private final Set<String> roles;
  private final Set<String> authorities;

  /**
   * Creates a principal.
   *
   * @param userName the user name
   * @param roles the roles of the user
   * @param authorities the authorities granted to the user
   */
  public BBjPrincipal(String userName, Set<String> roles, Set<String> authorities) {
    this.userName = userName;
    this.roles = roles;
    this.authorities = authorities;
  }

  /**
   * Gets the user name.
   *
   * @return the user name
   */
  public String getUserName() {
    return userName;
  }

  /**
   * Gets the roles of the user.
   *
   * @return the roles
   */
  public Set<String> getRoles() {
    return roles;
  }

  /**
   * Gets the authorities granted to the user.
   *
   * @return the authorities
   */
  public Set<String> getAuthorities() {
    return authorities;
  }

}
