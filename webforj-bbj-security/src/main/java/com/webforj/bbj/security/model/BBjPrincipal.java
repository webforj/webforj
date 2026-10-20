package com.webforj.bbj.security.model;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;

/**
 * The signed-in BBj user of an app.
 *
 * <p>
 * Holds the username and the names of the BBj security groups of the user as roles.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public final class BBjPrincipal {
  private final String userName;
  private final Set<String> roles;

  /**
   * Creates a principal.
   *
   * @param userName the BBj username
   * @param roles the BBj security group names
   *
   * @throws NullPointerException if an argument or a role is {@code null}
   */
  public BBjPrincipal(String userName, Collection<String> roles) {
    this.userName = Objects.requireNonNull(userName, "userName");
    this.roles = Set.copyOf(Objects.requireNonNull(roles, "roles"));
  }

  /**
   * Gets the BBj username.
   *
   * @return the username
   */
  public String getUserName() {
    return userName;
  }

  /**
   * Gets the BBj security group names of the user.
   *
   * @return the unmodifiable roles
   */
  public Set<String> getRoles() {
    return roles;
  }
}
