package com.webforj.bbjship;

import java.io.IOException;

/**
 * Reports a deployment BBjShip refuses because of its name, its archive or the applications already
 * published.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentException extends IOException {

  /**
   * Names why a deployment was refused.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  enum Reason {
    /** The publish name is invalid. */
    INVALID_NAME,
    /** The archive does not have the deployment layout. */
    INVALID_ARCHIVE,
    /** The archive exceeds the allowed size. */
    ARCHIVE_TOO_LARGE,
    /** The name belongs to another application. */
    NAME_CONFLICT,
    /** Another name publishes the same entry class. */
    ENTRY_CONFLICT,
    /** BBjShip published no application of the name. */
    NOT_FOUND,
    /** The name is published in the other mode, archive deployment or development run. */
    MODE_CONFLICT,
    /** The upload is a development run and development runs are not enabled. */
    DEVELOPMENT_DISABLED
  }

  private static final long serialVersionUID = 1L;

  private final Reason reason;

  DeploymentException(Reason reason, String message) {
    super(message);
    this.reason = reason;
  }

  DeploymentException(Reason reason, String message, Throwable cause) {
    super(message, cause);
    this.reason = reason;
  }

  Reason getReason() {
    return reason;
  }
}
