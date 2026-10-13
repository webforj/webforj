package com.webforj.bbjship;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminFactory;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;

/**
 * Opens an administration session of the BBjServices of this machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjServicesAdminSessionFactory implements AdminSessionFactory {

  /**
   * {@inheritDoc}
   */
  @Override
  public BBjAdminBase create(BBjCredentials credentials) throws BBjAdminException {
    if (credentials.isToken()) {
      return BBjAdminFactory.getBBjAdmin(credentials.getToken());
    }

    return BBjAdminFactory.getBBjAdmin(credentials.getUsername(), credentials.getPassword());
  }
}
