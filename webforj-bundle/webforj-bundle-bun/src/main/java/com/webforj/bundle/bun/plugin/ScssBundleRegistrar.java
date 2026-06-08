package com.webforj.bundle.bun.plugin;

import java.util.List;
import java.util.Set;

/**
 * Built in extension that compiles SCSS and Sass sources through a Bun build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class ScssBundleRegistrar extends CuratedBundleRegistrar {

  /**
   * Creates the SCSS extension.
   */
  public ScssBundleRegistrar() {
    super("webforj-scss", Set.of("scss", "sass"), "scss.mjs", List.of(pkg("sass", "^1.100.0")));
  }
}
