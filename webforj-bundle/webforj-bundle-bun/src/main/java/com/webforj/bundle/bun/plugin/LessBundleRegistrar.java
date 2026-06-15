package com.webforj.bundle.bun.plugin;

import java.util.List;
import java.util.Set;

/**
 * Built in extension that compiles LESS sources through a Bun build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class LessBundleRegistrar extends CuratedBundleRegistrar {

  /**
   * Creates the LESS extension.
   */
  public LessBundleRegistrar() {
    super("webforj-less", Set.of("less"), "less.mjs", List.of(pkg("less", "^4.6.4")));
  }
}
