package com.webforj.plugin.gradle;

import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.RegularFileProperty;
import org.gradle.api.provider.ListProperty;
import org.gradle.api.provider.MapProperty;
import org.gradle.api.provider.Property;

/**
 * Configures the webforJ bundler for a Gradle build.
 *
 * <p>
 * Every value mirrors a parameter of the webforJ Maven plugin, so a project moving between the two
 * build tools configures the bundler the same way.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public abstract class WebforjExtension {

  /**
   * The Bun version to download when no override is supplied.
   *
   * <p>
   * Pinned per project for build reproducibility.
   * </p>
   *
   * @return the Bun version property
   */
  public abstract Property<String> getBunVersion();

  /**
   * Optional path to an existing Bun binary.
   *
   * <p>
   * When set, the plugin uses this binary directly instead of downloading.
   * </p>
   *
   * @return the Bun binary property
   */
  public abstract RegularFileProperty getBunPath();

  /**
   * Cache root for the managed Bun binaries.
   *
   * <p>
   * Defaults to {@code ${user.home}/.webforj/bun}.
   * </p>
   *
   * @return the cache directory property
   */
  public abstract DirectoryProperty getCacheDir();

  /**
   * Source root for bundle entry files.
   *
   * @return the source root property
   */
  public abstract DirectoryProperty getSourceRoot();

  /**
   * Working directory for the generated build driver, its configuration, the plugin wrappers, the
   * build metafile, and the watch staging output.
   *
   * <p>
   * The package.json and the node_modules tree are installed at the project root, not here.
   * </p>
   *
   * @return the working directory property
   */
  public abstract DirectoryProperty getWorkDir();

  /**
   * Per extension enablement overrides keyed by extension id.
   *
   * <p>
   * Each extension decides whether it runs by default, an extension that handles a file type when a
   * matching file is present and one like Tailwind by staying off. An entry here flips that
   * default. Set {@code webforj-tailwind} to {@code true} to turn on an extension that is off by
   * default, or set {@code webforj-scss} to {@code false} to turn one off.
   * </p>
   *
   * @return the extension overrides property
   */
  public abstract MapProperty<String, String> getPlugins();

  /**
   * Package prefixes to skip during the annotation scan, on top of the built in exclusions for the
   * platform and common libraries.
   *
   * <p>
   * For example {@code com.acme.internal} stops the scan from parsing a large package that never
   * declares a bundle entry.
   * </p>
   *
   * @return the excluded packages property
   */
  public abstract ListProperty<String> getExcludePackages();

  /**
   * Whether to build a single eager bundle the runtime loads at app start, rather than per
   * component output.
   *
   * <p>
   * Required in a deployment that serves no static folder of its own, where the per component files
   * cannot be served and the bundle is inlined from the classpath instead. Also a scale choice on
   * the server side.
   * </p>
   *
   * @return the eager property
   */
  public abstract Property<Boolean> getEager();

  /**
   * Extra arguments appended to the {@code bun test} invocation.
   *
   * <p>
   * For example {@code --reporter=junit} together with an absolute {@code --reporter-outfile} path
   * writes a JUnit report for a continuous integration server. A path that writes a file must be
   * absolute, such as one under {@code project.build.directory}, because Bun runs from the frontend
   * source root and does not create the directory itself.
   * </p>
   *
   * @return the test arguments property
   */
  public abstract ListProperty<String> getTestArgs();
}
