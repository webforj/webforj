package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.foundation.model.BBjCredentials;

/**
 * Opens an administration session of BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@FunctionalInterface
public interface AdminSessionFactory {

  /**
   * Opens a session with the user name and password or the authorization token of the credentials.
   *
   * @param credentials the administration credentials
   *
   * @return the authenticated administration API
   * @throws BBjAdminException if BBjServices refuses the credentials or cannot be reached
   */
  BBjAdminBase create(BBjCredentials credentials) throws BBjAdminException;
}
