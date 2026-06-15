package com.webforj.plugin.gradle;

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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.gradle.api.DefaultTask;
import org.gradle.api.file.ConfigurableFileCollection;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Internal;
import org.gradle.work.DisableCachingByDefault;

/**
 * Shared collaborators and project derived inputs for the bundle, watch, and frontend test tasks.
 *
 * <p>
 * The user configuration lives on {@link WebforjExtension}, which is the single place a new option
 * is declared. Each task reads it through {@link #getExtension()}, so an option reaches the build
 * without being repeated here. Only the inputs the plugin derives from the Gradle project, the
 * classpath and the output locations, are declared on the task.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@DisableCachingByDefault(because = "not worth caching")
public abstract class AbstractBundlerTask extends DefaultTask {

  /**
   * The user configuration, the single source of the bundler options.
   *
   * @return the extension property
   */
  @Internal
  public abstract Property<WebforjExtension> getExtension();

  /**
   * The directory the bundle index and the served frontend assets are written into, so they are
   * packaged with the application.
   *
   * @return the classes output directory property
   */
  @Internal
  public abstract DirectoryProperty getClassesOutputDir();

  /**
   * The directory holding the package.json and the node_modules tree, the project root.
   *
   * @return the npm root property
   */
  @Internal
  public abstract DirectoryProperty getNpmRoot();

  /**
   * The compiled output and resolved dependencies scanned for bundle declarations.
   *
   * @return the project classpath collection
   */
  @Internal
  public abstract ConfigurableFileCollection getProjectClasspath();

  /**
   * The authored source roots a source scanning extension reads, without the generated output.
   *
   * @return the source scan roots collection
   */
  @Internal
  public abstract ConfigurableFileCollection getSourceScanRoots();

  /**
   * The project name, recorded as the package name in the generated package.json.
   *
   * @return the project name property
   */
  @Internal
  public abstract Property<String> getProjectName();

  /**
   * Builds the request payload from the user configuration and the project derived inputs.
   *
   * @return the request consumed by {@link BundlerExecution}
   */
  protected BundlerExecution.Request createRequest() {
    WebforjExtension config = getExtension().get();
    List<File> classpathRoots = new ArrayList<>(getProjectClasspath().getFiles());
    File classesOut = getClassesOutputDir().get().getAsFile();

    return new BundlerExecution.Request().setProjectName(getProjectName().get())
        .setClasspathRoots(classpathRoots)
        .setBundleSourceRoot(config.getSourceRoot().get().getAsFile().toPath())
        .setWorkDir(config.getWorkDir().get().getAsFile().toPath())
        .setClassesOutputDir(classesOut.toPath())
        .setNpmRoot(getNpmRoot().get().getAsFile().toPath())
        .setExtensionOverrides(toOverrides(config.getPlugins().get()))
        .setSourceScanRoots(sourceScanRoots())
        .setExcludedPackages(config.getExcludePackages().getOrElse(List.of()))
        .setEager(config.getEager().getOrElse(false));
  }

  /**
   * Builds the bundler execution from its collaborators and the configured Bun runtime.
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
   * Builds the configured Bun runtime.
   *
   * @return a runtime ready to resolve and execute Bun
   */
  protected BunRuntime createRuntime() {
    WebforjExtension config = getExtension().get();
    Path cache = config.getCacheDir().get().getAsFile().toPath();
    Path override =
        config.getBunPath().isPresent() ? config.getBunPath().get().getAsFile().toPath() : null;
    String version = config.getBunVersion().getOrNull();

    return BunRuntime.create().setCacheRoot(cache).setVersion(version).setOverridePath(override)
        .build();
  }

  private List<Path> sourceScanRoots() {
    List<Path> roots = new ArrayList<>();
    for (File dir : getSourceScanRoots().getFiles()) {
      roots.add(dir.toPath().toAbsolutePath().normalize());
    }

    return roots;
  }

  private static Map<String, Boolean> toOverrides(Map<String, String> plugins) {
    if (plugins == null || plugins.isEmpty()) {
      return Map.of();
    }

    Map<String, Boolean> overrides = new LinkedHashMap<>();
    plugins.forEach((id, value) -> overrides.put(id, Boolean.parseBoolean(value)));

    return overrides;
  }
}
