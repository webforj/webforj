package com.webforj.devtools.craftforj.router.model;

/**
 * Route type.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum RouteType {
  /** View route with a path. */
  VIEW,
  /** Layout route (pathless, wraps child routes). */
  LAYOUT
}
