package com.webforj.bundle.bun;

import java.io.IOException;

/**
 * The contract a bundler extension implements, the single way to extend the build.
 *
 * <p>
 * An extension is discovered on the project classpath through the {@link java.util.ServiceLoader},
 * so a feature plugs in by shipping an implementation and a service registration. It carries its
 * own identity through {@link #getId()}, decides whether it runs by default through
 * {@link #isEnabledByDefault(BundleContext)}, and contributes to the build through
 * {@link #onWillBundle(BundleContext)}. A project turns any extension on or off by id through the
 * build configuration, which overrides the default.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public interface BundleExtension {

  /**
   * Gets the stable id the extension is enabled or disabled by.
   *
   * @return the extension id, never null
   */
  String getId();

  /**
   * Indicates whether the extension runs when a project does not set it explicitly.
   *
   * <p>
   * An extension that ships always present uses this to stay off until asked for, or to turn itself
   * on only when the source it handles is there. A project override by id wins over this default.
   * </p>
   *
   * @param context the bundle context
   * @return {@code true} to run by default
   */
  default boolean isEnabledByDefault(BundleContext context) {
    return true;
  }

  /**
   * Called before the install and the build, once per build, when the extension is enabled.
   *
   * <p>
   * This is where an extension adds the npm packages it needs, the build plugins it ships, and the
   * entries it generated, through {@link BundleContext#addPackage},
   * {@link BundleContext#addPlugin}, and {@link BundleContext#addEntry}.
   * </p>
   *
   * @param context the bundle context
   * @throws IOException if writing a generated source fails
   */
  default void onWillBundle(BundleContext context) throws IOException {
    // no-op
  }

  /**
   * Called after a build finishes and its index is written, and again after every watch rebuild,
   * when the extension is enabled.
   *
   * <p>
   * {@link BundleContext#isRebuild()} tells the first build from a watch rebuild.
   * </p>
   *
   * @param context the bundle context
   */
  default void onDidBundle(BundleContext context) {
    // no-op
  }
}
