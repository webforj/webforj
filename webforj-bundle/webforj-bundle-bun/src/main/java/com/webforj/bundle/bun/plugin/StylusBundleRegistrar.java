package com.webforj.bundle.bun.plugin;

import java.util.List;
import java.util.Set;

/**
 * Built in extension that compiles Stylus sources through a Bun build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class StylusBundleRegistrar extends CuratedBundleRegistrar {

  /**
   * Creates the Stylus extension.
   */
  public StylusBundleRegistrar() {
    super("webforj-stylus", Set.of("styl"), "stylus.mjs", List.of(pkg("stylus", "^0.64.0")));
  }
}
