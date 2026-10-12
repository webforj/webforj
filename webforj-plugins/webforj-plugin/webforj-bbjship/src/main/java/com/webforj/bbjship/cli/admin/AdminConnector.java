package com.webforj.bbjship.cli.admin;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;

/**
 * Opens an authenticated administration session of BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
public interface AdminConnector {

  /**
   * Opens a session with the given credentials.
   *
   * @param username the administrator user name
   * @param password the administrator password
   *
   * @return the authenticated session
   * @throws BBjAdminException if BBjServices refuses the credentials or cannot be reached
   */
  BBjAdminBase open(String username, String password) throws BBjAdminException;
}
