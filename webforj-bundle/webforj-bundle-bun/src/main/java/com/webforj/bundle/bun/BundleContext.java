package com.webforj.bundle.bun;

import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import com.webforj.bundle.bun.plugin.BunPlugin;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The context a {@link BundleExtension} works from, built by the bundler and passed to each event.
 *
 * <p>
 * An extension reads the build locations through the {@code Path} accessors, inspects the present
 * source extensions, scans the classpath it needs, writes any generated source under
 * {@link #getGeneratedPath()}, and contributes what the build installs and builds through
 * {@link #addPackage(BundlePackageDeclaration)}, {@link #addPlugin(String, String)}, and
 * {@link #addEntry(BundleEntryDeclaration)}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleContext {
  private Set<URI> classpath = Set.of();
  private Path frontendPath = null;
  private List<Path> sourcePaths = List.of();
  private Path generatedPath = null;
  private Path outputPath = null;
  private Set<String> sourceExtensions = Set.of();
  private boolean production = false;
  private boolean rebuild = false;
  private BundleLogger log = null;
  private final List<BundlePackageDeclaration> packages = new ArrayList<>();
  private final Map<String, BundleEntryDeclaration> entries = new LinkedHashMap<>();
  private final Map<String, Set<String>> bindings = new LinkedHashMap<>();
  private final Map<String, BunPlugin> plugins = new LinkedHashMap<>();
  private final Set<Path> watchPaths = new LinkedHashSet<>();

  BundleContext() {}

  void setClasspath(Set<URI> classpath) {
    this.classpath = classpath;
  }

  /**
   * Gets the classpath the extension scans for its declarations, the compiled classes and the
   * dependency jars.
   *
   * @return the classpath
   */
  public Set<URI> getClasspath() {
    return classpath;
  }

  void setFrontendPath(Path frontendPath) {
    this.frontendPath = frontendPath;
  }

  /**
   * Gets the frontend source root the build compiles from.
   *
   * @return the frontend source root
   */
  public Path getFrontendPath() {
    return frontendPath;
  }

  void setSourcePaths(List<Path> sourcePaths) {
    this.sourcePaths = sourcePaths == null ? List.of() : List.copyOf(sourcePaths);
  }

  /**
   * Gets the application source roots, the trees an extension scans for source text, never null.
   *
   * @return the application source roots
   */
  public List<Path> getSourcePaths() {
    return sourcePaths;
  }

  void setGeneratedPath(Path generatedPath) {
    this.generatedPath = generatedPath;
  }

  /**
   * Gets the path the extension writes generated sources into.
   *
   * <p>
   * The path is cleared before the extensions run, so an extension writes into a subfolder of its
   * own.
   * </p>
   *
   * @return the generated sources path
   */
  public Path getGeneratedPath() {
    return generatedPath;
  }

  void setOutputPath(Path outputPath) {
    this.outputPath = outputPath;
  }

  /**
   * Gets the path the build writes its output to.
   *
   * @return the output path
   */
  public Path getOutputPath() {
    return outputPath;
  }

  void setSourceExtensions(Set<String> sourceExtensions) {
    this.sourceExtensions = sourceExtensions == null ? Set.of() : Set.copyOf(sourceExtensions);
  }

  /**
   * Gets the lower case file extensions present under the frontend source root, without the leading
   * dot, so an extension turns itself on only when the source it handles is there.
   *
   * @return the present source extensions, never null
   */
  public Set<String> getSourceExtensions() {
    return sourceExtensions;
  }

  void setProduction(boolean production) {
    this.production = production;
  }

  /**
   * Indicates whether the build is a production run.
   *
   * @return {@code true} for a production run
   */
  public boolean isProduction() {
    return production;
  }

  void setRebuild(boolean rebuild) {
    this.rebuild = rebuild;
  }

  /**
   * Indicates whether the build is a watch rebuild rather than the first build.
   *
   * @return {@code true} for a watch rebuild
   */
  public boolean isRebuild() {
    return rebuild;
  }

  void setLog(BundleLogger log) {
    this.log = log;
  }

  /**
   * Gets the build log, so an extension writes its progress through the same channel as the build.
   * A line logged here appears on the build console, and is forwarded to the running application
   * during a watch.
   *
   * @return the build log
   */
  public BundleLogger getLog() {
    return log;
  }

  /**
   * Adds an npm package the extension needs installed.
   *
   * @param declaration the package declaration
   */
  public void addPackage(BundlePackageDeclaration declaration) {
    packages.add(declaration);
  }

  /**
   * Adds a Bun build plugin the extension ships.
   *
   * <p>
   * The wrapper source is a module that default-exports a factory returning a ready
   * {@code Bun.build} plugin. The build writes it next to the driver and loads it, passing the per
   * id options a project declared in its {@code bun.config.ts}. The npm packages the wrapper
   * imports are added through {@link #addPackage(BundlePackageDeclaration)}.
   * </p>
   *
   * @param id the plugin id, also the key the project passes options by
   * @param wrapperSource the wrapper module source
   */
  public void addPlugin(String id, String wrapperSource) {
    plugins.put(id, new BunPlugin().setId(id).setWrapper(id + "/" + id + ".mjs")
        .setWrapperContent(wrapperSource.getBytes(StandardCharsets.UTF_8)));
  }

  /**
   * Adds a generated entry the build compiles.
   *
   * <p>
   * An entry that carries owners loads for those routed classes. An entry with no owners loads for
   * every view.
   * </p>
   *
   * @param declaration the entry declaration, its source the key the build records it under
   */
  public void addEntry(BundleEntryDeclaration declaration) {
    entries.putIfAbsent(declaration.getSource(), declaration);
    List<String> owners = declaration.getOwners();
    if (owners.isEmpty()) {
      bind(BundleIndex.GLOBAL_KEY, declaration.getSource());
    } else {
      for (String owner : owners) {
        bind(owner, declaration.getSource());
      }
    }
  }

  private void bind(String key, String source) {
    bindings.computeIfAbsent(key, name -> new LinkedHashSet<>()).add(source);
  }

  /**
   * Adds a directory the development watch rebuilds on when its contents change.
   *
   * <p>
   * An extension that builds from sources outside the frontend root, such as a stylesheet generated
   * from the classes a project uses, registers those sources here so editing them regenerates the
   * bundle during a watch.
   * </p>
   *
   * @param path the directory to watch
   */
  public void addWatchPath(Path path) {
    watchPaths.add(path);
  }

  Set<Path> getWatchPaths() {
    return watchPaths;
  }

  List<BundlePackageDeclaration> getPackages() {
    return packages;
  }

  List<BundleEntryDeclaration> getEntries() {
    return new ArrayList<>(entries.values());
  }

  Map<String, Set<String>> getBindings() {
    return bindings;
  }

  List<BunPlugin> getPlugins() {
    return new ArrayList<>(plugins.values());
  }
}
