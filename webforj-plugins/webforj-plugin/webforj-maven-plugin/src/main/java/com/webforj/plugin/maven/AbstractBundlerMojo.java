package com.webforj.plugin.maven;

import com.webforj.bundle.bun.BundlerExecution;
import com.webforj.bundle.bun.discovery.BundleEntryResolver;
import com.webforj.bundle.bun.discovery.ClasspathPackageScanner;
import com.webforj.bundle.bun.runtime.BunRuntime;
import com.webforj.bundle.bun.writer.BundleDriverWriter;
import com.webforj.bundle.bun.writer.BundleIndexWriter;
import com.webforj.bundle.bun.writer.PackageJsonWriter;
import java.io.File;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

/**
 * Common parameters shared by the bundle and watch goals.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public abstract class AbstractBundlerMojo extends AbstractMojo {

  /** The current Maven project. */
  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  protected MavenProject project;

  /**
   * The Bun version to download when no override is supplied.
   *
   * <p>
   * Pinned per project for build reproducibility.
   * </p>
   */
  @Parameter(property = "webforj.bundler.version")
  protected String bunVersion;

  /**
   * Optional path to an existing Bun binary.
   *
   * <p>
   * When set, the plugin uses this binary directly instead of downloading.
   * </p>
   */
  @Parameter(property = "webforj.bundler.path")
  protected File bunPath;

  /**
   * Cache root for the managed Bun binaries.
   *
   * <p>
   * Defaults to {@code ${user.home}/.webforj/bun}.
   * </p>
   */
  @Parameter(property = "webforj.bundler.cacheDir", defaultValue = "${user.home}/.webforj/bun")
  protected File cacheDir;

  /**
   * Source root for bundle entry files.
   */
  @Parameter(property = "webforj.bundler.sourceRoot",
      defaultValue = "${project.basedir}/src/main/frontend")
  protected File sourceRoot;

  /**
   * Working directory for the generated build driver, its configuration, the plugin wrappers, the
   * build metafile, and the watch staging output.
   *
   * <p>
   * The package.json and the node_modules tree are installed at the project root, not here.
   * </p>
   */
  @Parameter(property = "webforj.bundler.workDir",
      defaultValue = "${project.build.directory}/bundle")
  protected File workDir;

  /**
   * Per extension enablement overrides keyed by extension id.
   *
   * <p>
   * Each extension decides whether it runs by default, an extension that handles a file type when a
   * matching file is present and one like Tailwind by staying off. An entry here flips that
   * default. Set {@code webforj-tailwind} to {@code true} to turn on an extension that is off by
   * default, or set {@code webforj-scss} to {@code false} to turn one off.
   * </p>
   */
  @Parameter(alias = "plugins")
  protected Map<String, String> plugins;

  /**
   * Package prefixes to skip during the annotation scan, on top of the built in exclusions for the
   * platform and common libraries.
   *
   * <p>
   * For example {@code com.acme.internal} stops the scan from parsing a large package that never
   * declares a bundle entry.
   * </p>
   */
  @Parameter(property = "webforj.bundler.excludePackages")
  protected String[] excludePackages;

  /**
   * Whether to build a single eager bundle the runtime loads at app start, rather than per
   * component output.
   *
   * <p>
   * Required in a deployment that serves no static folder of its own, where the per component files
   * cannot be served and the bundle is inlined from the classpath instead. Also a scale choice on
   * the server side.
   * </p>
   */
  @Parameter(property = "webforj.bundler.eager", defaultValue = "false")
  protected boolean eager;

  /**
   * Creates the request payload from the resolved Mojo parameters.
   *
   * @return the request consumed by {@link BundlerExecution}
   */
  protected BundlerExecution.Request createRequest() {
    List<File> classpathRoots = new ArrayList<>();
    File classesDir = new File(project.getBuild().getOutputDirectory());
    if (classesDir.exists()) {
      classpathRoots.add(classesDir);
    }

    project.getArtifacts().forEach(artifact -> {
      if (artifact.getFile() != null) {
        classpathRoots.add(artifact.getFile());
      }
    });

    return new BundlerExecution.Request().setProjectArtifactId(project.getArtifactId())
        .setClasspathRoots(classpathRoots).setBundleSourceRoot(sourceRoot.toPath())
        .setWorkDir(workDir.toPath()).setClassesOutputDir(classesDir.toPath())
        .setNpmRoot(project.getBasedir().toPath()).setExtensionOverrides(toOverrides(plugins))
        .setSourceScanRoots(sourceScanRoots())
        .setExcludedPackages(excludePackages == null ? null : Arrays.asList(excludePackages))
        .setEager(eager);
  }

  /**
   * Creates the bundler execution from its collaborators and the configured Bun runtime.
   *
   * @return the bundler execution
   */
  protected BundlerExecution createExecution() {
    return BundlerExecution.create().setScanner(new ClasspathPackageScanner())
        .setResolver(new BundleEntryResolver()).setPackageJsonWriter(new PackageJsonWriter())
        .setIndexWriter(new BundleIndexWriter()).setDriverWriter(new BundleDriverWriter())
        .setBunRuntime(createRuntime()).build();
  }

  /**
   * Creates the configured Bun runtime.
   *
   * @return a runtime ready to resolve and execute Bun
   */
  protected BunRuntime createRuntime() {
    Path cache = cacheDir.toPath();
    Path override = bunPath == null ? null : bunPath.toPath();

    return BunRuntime.create().setCacheRoot(cache).setVersion(bunVersion).setOverridePath(override)
        .build();
  }

  private static Map<String, Boolean> toOverrides(Map<String, String> plugins) {
    if (plugins == null || plugins.isEmpty()) {
      return Map.of();
    }

    Map<String, Boolean> overrides = new LinkedHashMap<>();
    plugins.forEach((id, value) -> overrides.put(id, Boolean.parseBoolean(value)));

    return overrides;
  }

  private List<Path> sourceScanRoots() {
    List<String> dirs = project.getCompileSourceRoots();
    if (dirs == null) {
      return List.of();
    }

    // The compile source roots include the annotation processor output under the build directory,
    // which is regenerated every build and not authored source. Leave it out so a source scanning
    // extension reads only the application sources.
    Path buildDir = buildDirectory();
    List<Path> roots = new ArrayList<>();
    for (String dir : dirs) {
      Path path = new File(dir).toPath().toAbsolutePath().normalize();
      if (buildDir == null || !path.startsWith(buildDir)) {
        roots.add(path);
      }
    }

    return roots;
  }

  private Path buildDirectory() {
    String dir = project.getBuild() == null ? null : project.getBuild().getDirectory();

    return dir == null ? null : new File(dir).toPath().toAbsolutePath().normalize();
  }
}
