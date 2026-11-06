package com.webforj.bbj.security.model;

import java.util.Locale;

/**
 * A system permission of a BBj user.
 *
 * <p>
 * {@link #toString()} gives the name of the permission in BBj.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
public enum BBjPermissions {
  /** The permission {@code user}. */
  USER,

  /** The permission {@code add-user}. */
  ADD_USER,

  /** The permission {@code attach-existing-db}. */
  ATTACH_EXISTING_DB,

  /** The permission {@code set-permissions}. */
  SET_PERMISSIONS,

  /** The permission {@code bbj-processes}. */
  BBJ_PROCESSES,

  /** The permission {@code create-trigger}. */
  CREATE_TRIGGER,

  /** The permission {@code remove-user}. */
  REMOVE_USER,

  /** The permission {@code create-new-db}. */
  CREATE_NEW_DB,

  /** The permission {@code admin-access}. */
  ADMIN_ACCESS,

  /** The permission {@code set-passwords}. */
  SET_PASSWORDS,

  /** The permission {@code drop-db}. */
  DROP_DB,

  /** The permission {@code sql-connections}. */
  SQL_CONNECTIONS,

  /** The permission {@code server-config}. */
  SERVER_CONFIG,

  /** The permission {@code connection-pools}. */
  CONNECTION_POOLS,

  /** The permission {@code copy-jobs}. */
  COPY_JOBS,

  /** The permission {@code table-analysis-queue}. */
  TABLE_ANALYSIS_QUEUE,

  /** The permission {@code force-close-files}. */
  FORCE_CLOSE_FILES,

  /** The permission {@code replication}. */
  REPLICATION,

  /** The permission {@code scheduling}. */
  SCHEDULING,

  /** The permission {@code auditing}. */
  AUDITING,

  /** The permission {@code asynch-trigger}. */
  ASYNCH_TRIGGER,

  /** The permission {@code document-archives}. */
  DOCUMENT_ARCHIVES,

  /** The permission {@code email-services}. */
  EMAIL_SERVICES,

  /** The permission {@code metrics}. */
  METRICS,

  /** The permission {@code file-manager}. */
  FILE_MANAGER,

  /** The permission {@code view-open-files}. */
  VIEW_OPEN_FILES;

  /**
   * Gets the name of the permission in BBj.
   *
   * @return the name of the permission in BBj
   */
  @Override
  public String toString() {
    return name().toLowerCase(Locale.ROOT).replace('_', '-');
  }
}
