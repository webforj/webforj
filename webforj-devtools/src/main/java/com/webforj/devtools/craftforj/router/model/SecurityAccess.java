package com.webforj.devtools.craftforj.router.model;

/**
 * Security access type for a route.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum SecurityAccess {
  /**
   * No security annotation present.
   */
  NONE,

  /**
   * Permit all annotation.
   */
  PERMIT_ALL,

  /**
   * Deny all annotation.
   */
  DENY_ALL,

  /**
   * Roles allowed annotation.
   */
  ROLES_ALLOWED,

  /**
   * Anonymous access annotation.
   */
  ANONYMOUS
}
