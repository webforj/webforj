package com.webforj.bundle.bun.plugin;

import java.util.List;
import java.util.Set;

/**
 * Built in extension that compiles Svelte components through a Bun build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class SvelteBundleRegistrar extends CuratedBundleRegistrar {

  /**
   * Creates the Svelte extension.
   */
  public SvelteBundleRegistrar() {
    super("webforj-svelte", Set.of("svelte"), "svelte.mjs",
        List.of(pkg("bun-plugin-svelte", "^0.0.6"), pkg("svelte", "^5.56.2")));
  }
}
