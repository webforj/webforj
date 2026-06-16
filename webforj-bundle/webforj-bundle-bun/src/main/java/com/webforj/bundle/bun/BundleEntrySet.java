package com.webforj.bundle.bun;

import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import com.webforj.bundle.bun.discovery.ClasspathPackageScanner;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * The set of inputs a bundle is built from, captured so two scans can be compared by value.
 *
 * <p>
 * A development restart can change which entries the build covers. Comparing the entries, the debug
 * only entries, the declared npm packages and the class to entry bindings of a fresh scan against
 * the set the running watcher was built from tells the watch whether the restart changed anything.
 * </p>
 *
 * @param entries the entry sources to build
 * @param debugEntries the entry sources built only for the development bundle
 * @param packages the declared npm packages as {@code name@version} coordinates, suffixed with
 *        {@code :dev} for a development only package
 * @param bindings the routed class to entry key bindings
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
// @formatter:off
record BundleEntrySet(Set<String> entries, Set<String> debugEntries, Set<String> packages,
    Map<String, Set<String>> bindings) {

  /**
   * Captures the entry set of a scan.
   *
   * @param scan the scan to capture
   * @return the entry set
   */
  static BundleEntrySet from(ClasspathPackageScanner.Result scan) {
    Set<String> packages = new HashSet<>();
    for (BundlePackageDeclaration declaration : scan.getPackages()) {
      packages.add(declaration.getName() + "@" + declaration.getVersion()
          + (declaration.isDev() ? ":dev" : ""));
    }

    return new BundleEntrySet(new HashSet<>(scan.getSources()),
        new HashSet<>(scan.getDebugSources()), packages, new HashMap<>(scan.getBindings()));
  }

  /**
   * Indicates whether there is nothing to build, no entry source and no debug only entry source.
   *
   * @return {@code true} when there are no entries to build
   */
  boolean isEmpty() {
    return entries.isEmpty() && debugEntries.isEmpty();
  }
}
// @formatter:on
