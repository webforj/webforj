package com.webforj.devtools.craftforj.styles.model;

/**
 * Where a region is put when the stylesheet does not already carry it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum RegionPlacement {
  /** After any leading {@code @charset} and {@code @import}, ahead of the app's own rules. */
  START,

  /** At the end of the file, after everything the app wrote. */
  END
}
