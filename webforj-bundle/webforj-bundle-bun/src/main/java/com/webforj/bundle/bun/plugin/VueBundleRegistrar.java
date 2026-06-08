package com.webforj.bundle.bun.plugin;

import java.util.List;
import java.util.Set;

/**
 * Built in extension that compiles Vue single file components through a Bun build plugin.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class VueBundleRegistrar extends CuratedBundleRegistrar {

  /**
   * Creates the Vue extension.
   */
  public VueBundleRegistrar() {
    super("webforj-vue", Set.of("vue"), "vue.mjs",
        List.of(pkg("bun-plugin-vue3", "^1.1.0"), pkg("typescript", "^6.0.3")));
  }
}
