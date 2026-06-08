package com.webforj.bundle.bun;

import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.BundleIndexDocument;
import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.discovery.BundleEntryResolver;
import com.webforj.bundle.bun.discovery.BundlePackageDeclaration;
import com.webforj.bundle.bun.discovery.ClasspathPackageScanner;
import com.webforj.bundle.bun.discovery.DependencyEntryExtractor;
import com.webforj.bundle.bun.plugin.BunPlugin;
import com.webforj.bundle.bun.runtime.BunRuntime;
import com.webforj.bundle.bun.writer.BundleDriverWriter;
import com.webforj.bundle.bun.writer.BundleIndexWriter;
import com.webforj.bundle.bun.writer.PackageJsonWriter;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

/**
 * Carries out the end to end bundler workflow on behalf of the Mojos.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundlerExecution {
  private static final String OUTPUT_RESOURCE_PREFIX = "static/frontend/";
  private static final String WATCH_STAGING_DIR = "watch-staging";

  private static final String USER_CONFIG_NAME = "bun.config.ts";
  private static final String META_FILE = "meta.json";
  private static final String NPM_ENTRY_DIR = "generated";
  private static final String PACKAGE_JSON = "package.json";
  private static final String EAGER_ENTRY = "bundle.js";
  private static final Set<String> SCRIPT_EXTENSIONS =
      Set.of(".js", ".mjs", ".cjs", ".jsx", ".ts", ".tsx");
  private final BundleLog log = new BundleLog(LoggerFactory.getLogger(BundlerExecution.class));

  private final ClasspathPackageScanner scanner;
  private final BundleEntryResolver resolver;
  private final PackageJsonWriter packageJson;
  private final BundleIndexWriter indexWriter;
  private final BundleDriverWriter driverWriter;
  private final BunRuntime bun;

  BundlerExecution(BundlerExecutionBuilder builder) {
    this.scanner = builder.scanner;
    this.resolver = builder.resolver;
    this.packageJson = builder.packageJson;
    this.indexWriter = builder.indexWriter;
    this.driverWriter = builder.driverWriter;
    this.bun = builder.bunRuntime;
  }

  /**
   * Creates a new builder for an execution.
   *
   * @return a new builder
   */
  public static BundlerExecutionBuilder create() {
    return new BundlerExecutionBuilder();
  }

  /**
   * Runs a single shot production bundle with hashed, minified output.
   *
   * @param request the parameters for this run
   *
   * @return the served output directory, or {@code null} when nothing was bundled
   * @throws IOException on any IO failure
   * @throws InterruptedException if the run is interrupted
   */
  public Path run(Request request) throws IOException, InterruptedException {
    // A production build must never ship the development index, which a watch leaves under the
    // served static folder. Remove it up front so a packaged application can never carry it, even
    // when there is nothing to bundle and the build returns early.
    Files.deleteIfExists(
        request.getClassesOutputDir().resolve(BundleIndexDocument.DEVELOPMENT_RESOURCE));

    CompileContext compileContext = createCompileContext(request, true);
    Path servedDir = getServedOutputDir(request);
    Prepared prepared = compile(compileContext, servedDir);
    if (prepared == null) {
      return null;
    }

    writeIndex(request, prepared, false);
    notifyDidBundle(compileContext, servedDir, false);

    return servedDir;
  }

  /**
   * Runs a blocking initial development build with stable, unminified output and then start a
   * background Bun watcher that rebuilds in place on every source change.
   *
   * @param request the parameters for this run
   *
   * @return the running Bun watcher process, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public Process watch(Request request) throws IOException, InterruptedException {
    return watch(request, null, null);
  }

  /**
   * Runs a blocking initial development build and then starts a background Bun watcher, reporting
   * the files whose served output changed on every rebuild.
   *
   * @param request the parameters for this run
   * @param rebuildListener notified with the changed served paths after each rebuild, never empty,
   *        may be null
   *
   * @return the running Bun watcher process, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public Process watch(Request request, Consumer<List<String>> rebuildListener)
      throws IOException, InterruptedException {
    return watch(request, rebuildListener, null);
  }

  /**
   * Runs a blocking initial development build and then starts a background Bun watcher, reporting
   * the files whose served output changed on every rebuild and every line the watcher prints.
   *
   * @param request the parameters for this run
   * @param rebuildListener notified with the changed served paths after each rebuild, never empty,
   *        may be null
   * @param logListener notified with every bundler line and its level, may be null
   *
   * @return the running Bun watcher process, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public Process watch(Request request, Consumer<List<String>> rebuildListener,
      BiConsumer<Level, String> logListener) throws IOException, InterruptedException {
    // During a watch every bundler line is forwarded to the running application, so nothing is
    // written to the build console while the application is up.
    log.setSink(logListener);
    CompileContext compileContext = createCompileContext(request, false);
    Path servedDir = getServedOutputDir(request);
    Path stagingDir = request.getWorkDir().resolve(WATCH_STAGING_DIR);
    Prepared prepared = compile(compileContext, stagingDir);
    if (prepared == null) {
      return null;
    }

    // The driver rebuilds the whole graph and reemits every output on each change, so it writes to
    // a private staging directory. Only the files whose content actually changed are copied into
    // the directory the server serves. An edit that affects only a stylesheet therefore leaves the
    // scripts untouched, and the dev tools see a stylesheet change rather than a full page reload.
    Files.createDirectories(servedDir);
    syncChangedFiles(stagingDir, servedDir);

    // The watch runs in the Maven process, so the index is written to the project output once,
    // where the application reads it from the classpath. It is written under the static folder,
    // which the development servers exclude from their reload scan, so the application never
    // redeploys when it appears. Development output names are stable, so a content edit never
    // changes the index and the watcher leaves it untouched on every later rebuild.
    writeIndex(request, prepared, true);
    notifyDidBundle(compileContext, servedDir, false);

    List<String> watchArgs = new ArrayList<>(prepared.getRunArgs());
    watchArgs.add("--watch");
    log.info("starting bun watch, edit sources to rebuild");

    AtomicBoolean baselineEstablished = new AtomicBoolean(false);

    return bun.start(request.getBundleSourceRoot(), watchArgs, line -> {
      if (logListener != null) {
        logListener.accept(Level.INFO, line);
      }

      if (isRebuildComplete(line)) {
        List<String> changed = syncQuietly(stagingDir, servedDir);
        notifyDidBundle(compileContext, servedDir, true);
        // The watcher emits its own build when it starts. That first build is the baseline: it
        // realigns the served directory with the watch output and never reloads the browser. Every
        // later rebuild is driven by a developer edit and reaches the reload listener.
        if (baselineEstablished.compareAndSet(false, true)) {
          return;
        }

        if (rebuildListener != null && !changed.isEmpty()) {
          rebuildListener.accept(changed);
        }
      }
    });
  }

  /**
   * Runs the Bun test runner over the bundle source root.
   *
   * <p>
   * Declared npm packages are installed first so test files can import them. When the source root
   * holds no test files the run is skipped.
   * </p>
   *
   * @param request the parameters for this run
   *
   * @return the Bun test exit code, zero when there is nothing to test
   * @throws IOException on any IO failure
   * @throws InterruptedException if the run is interrupted
   */
  public int test(Request request) throws IOException, InterruptedException {
    Path sourceRoot = request.getBundleSourceRoot();
    if (sourceRoot == null || !Files.isDirectory(sourceRoot) || !hasTestFiles(sourceRoot)) {
      log.info("no frontend tests found, skipping bun test");

      return 0;
    }

    installDependencies(request);
    log.info("running bun test in {}", sourceRoot);

    List<String> args = new ArrayList<>();
    args.add("test");
    args.addAll(request.getTestArgs());

    return bun.execute(sourceRoot, args);
  }

  private CompileContext createCompileContext(Request request, boolean production) {
    List<BundleExtension> extensions = loadExtensions(request);
    Set<String> sourceExtensions = scanSourceExtensions(request.getBundleSourceRoot());

    return new CompileContext(request, extensions, sourceExtensions, production);
  }

  private List<BundleExtension> loadExtensions(Request request) {
    List<URL> urls = new ArrayList<>();
    for (File root : request.getClasspathRoots()) {
      if (root == null || !root.exists()) {
        continue;
      }

      try {
        urls.add(root.toURI().toURL());
      } catch (MalformedURLException e) {
        log.warn("skipping classpath root {}: {}", root, e.getMessage());
      }
    }

    // A child loader over the project classpath with the bundler's own loader as parent, so an
    // extension a project depends on is discovered while the extension type still resolves to the
    // bundler's copy. The bundler's own built in extensions resolve through the parent.
    URLClassLoader loader =
        new URLClassLoader(urls.toArray(URL[]::new), getClass().getClassLoader());
    List<BundleExtension> extensions = new ArrayList<>();
    for (BundleExtension extension : ServiceLoader.load(BundleExtension.class, loader)) {
      extensions.add(extension);
    }

    return extensions;
  }

  private Path getServedOutputDir(Request request) {
    return request.getClassesOutputDir().resolve(OUTPUT_RESOURCE_PREFIX);
  }

  private void notifyDidBundle(CompileContext compileContext, Path outputDir, boolean rebuild) {
    BundleContext context = createContext(compileContext, outputDir, rebuild);
    for (BundleExtension extension : compileContext.getExtensions()) {
      if (!isExtensionActive(extension, context,
          compileContext.getRequest().getExtensionOverrides())) {
        continue;
      }

      try {
        extension.onDidBundle(context);
      } catch (RuntimeException e) {
        log.warn("extension {} failed: {}", extension.getId(), e.getMessage());
      }
    }
  }

  private boolean isExtensionActive(BundleExtension extension, BundleContext context,
      Map<String, Boolean> overrides) {
    Boolean override = overrides.get(extension.getId());

    return override != null ? override : extension.isEnabledByDefault(context);
  }

  private BundleContext createContext(CompileContext compileContext, Path outputDir,
      boolean rebuild) {
    Request request = compileContext.getRequest();
    Path sourceRoot = request.getBundleSourceRoot();
    BundleContext context = new BundleContext();
    context.setClasspath(getUris(request.getClasspathRoots()));
    context.setFrontendPath(sourceRoot);
    context.setSourcePaths(request.getSourceScanRoots());
    context.setGeneratedPath(sourceRoot.resolve(NPM_ENTRY_DIR));
    context.setOutputPath(outputDir);
    context.setSourceExtensions(compileContext.getSourceExtensions());
    context.setProduction(compileContext.isProduction());
    context.setRebuild(rebuild);
    context.setLog(log);

    return context;
  }

  private Prepared compile(CompileContext compileContext, Path outputDir)
      throws IOException, InterruptedException {
    Prepared prepared = prepare(compileContext, outputDir);
    if (prepared == null) {
      return null;
    }

    int code =
        bun.execute(compileContext.getRequest().getBundleSourceRoot(), prepared.getRunArgs());
    if (code != 0) {
      throw new IOException("bun build failed with exit code " + code);
    }

    return prepared;
  }

  private Prepared prepare(CompileContext compileContext, Path outputDir)
      throws IOException, InterruptedException {
    Request request = compileContext.getRequest();
    Set<URI> classpath = getUris(request.getClasspathRoots());
    ClasspathPackageScanner.Result scan = scanClasspath(classpath, request.getExcludedPackages());

    Path sourceRoot = request.getBundleSourceRoot();
    Path generatedDir = sourceRoot.resolve(NPM_ENTRY_DIR);
    extractDependencyFrontend(classpath, generatedDir);

    List<BundleEntryDeclaration> entries =
        new ArrayList<>(prepareBaseEntries(resolveEntries(scan, sourceRoot, generatedDir),
            generatedDir, sourceRoot));

    BundleContext context = createContext(compileContext, outputDir, false);
    runExtensions(compileContext.getExtensions(), context, request.getExtensionOverrides());

    entries.addAll(context.getEntries());

    List<BunPlugin> plugins = context.getPlugins();
    if (entries.isEmpty()) {
      log.info("nothing to bundle, skipping Bun invocation");

      return null;
    }

    // In eager mode the whole frontend folds into one self contained bundle the runtime loads at
    // app start, so the entries become a single generated module and no class to output binding is
    // written. Otherwise each entry builds on its own and the runtime resolves a routed class to
    // its outputs through the class to entry key binding gathered from the scan and from the
    // extensions.
    boolean eager = request.isEager();
    List<BundleEntryDeclaration> driverEntries;
    Map<String, Set<String>> bindings;
    Set<String> debugSources;
    if (eager) {
      driverEntries = List.of(writeEagerEntry(generatedDir, entries));
      bindings = Map.of();
      debugSources = Set.of();
      log.info("eager mode, folding {} entries into a single bundle", entries.size());
    } else {
      driverEntries = entries;
      bindings = new LinkedHashMap<>();
      mergeBindings(bindings, scan.getBindings());
      mergeBindings(bindings, context.getBindings());
      debugSources = scan.getDebugSources();
    }

    Path workDir = request.getWorkDir();
    Files.createDirectories(workDir);
    List<BundlePackageDeclaration> declarations =
        mergeDeclarations(scan.getPackages(), context.getPackages());
    installAtRoot(request, declarations);

    prepareOutputDir(outputDir);

    Path metafile = workDir.resolve(META_FILE);
    List<String> runArgs = writeDriverFiles(request, workDir, driverEntries, outputDir, metafile,
        plugins, compileContext.isProduction(), !eager);

    return new Prepared().setMetafile(metafile).setRunArgs(runArgs).setBindings(bindings)
        .setDebugSources(debugSources).setEager(eager);
  }

  private ClasspathPackageScanner.Result scanClasspath(Set<URI> classpath,
      Set<String> excludedPackages) {
    ClasspathPackageScanner.Result scan = scanner.scan(classpath, excludedPackages);
    for (String warning : scan.getWarnings()) {
      log.warn(warning);
    }

    log.info("found {} npm package declaration(s)", scan.getPackages().size());
    log.info("found {} entry declaration(s)", scan.getSources().size());

    return scan;
  }

  private Set<URI> getUris(List<File> roots) {
    Set<URI> uris = new LinkedHashSet<>();
    for (File root : roots) {
      if (root == null || !root.exists()) {
        continue;
      }

      uris.add(root.toURI());
    }

    return uris;
  }

  private void extractDependencyFrontend(Set<URI> classpath, Path generatedDir) throws IOException {
    // A dependency can ship its own frontend inside its JAR. Extract it into the generated sources
    // directory before resolving entries, so the single build compiles it the same as local source.
    ensureGeneratedDir(generatedDir);
    int extracted = new DependencyEntryExtractor().extract(classpath, generatedDir);
    if (extracted > 0) {
      log.info("extracted {} frontend file(s) shipped by dependencies", extracted);
    }
  }

  private void ensureGeneratedDir(Path npmDir) throws IOException {
    if (Files.isDirectory(npmDir)) {
      clearDirectory(npmDir);
    }

    Files.createDirectories(npmDir);
    Files.writeString(npmDir.resolve(".gitignore"), "*\n");
  }

  private List<BundleEntryDeclaration> resolveEntries(ClasspathPackageScanner.Result scan,
      Path sourceRoot, Path generatedDir) {
    for (String missing : resolver.getUnresolved(sourceRoot, generatedDir, scan.getSources())) {
      log.warn("no source file under {} or a dependency for @BundleEntry '{}'", sourceRoot,
          missing);
    }

    List<BundleEntryDeclaration> resolved =
        resolver.resolve(sourceRoot, generatedDir, scan.getSources());
    log.info("resolved {} entry source(s) to build", resolved.size());

    return resolved;
  }

  private List<BundleEntryDeclaration> prepareBaseEntries(List<BundleEntryDeclaration> entries,
      Path generatedDir, Path sourceRoot) throws IOException {
    for (BundleEntryDeclaration entry : entries) {
      if (entry.isNpm()) {
        String stubPath = getNpmStubPath(entry.getSource());
        Path stubFile = generatedDir.resolve(stubPath);
        Files.createDirectories(stubFile.getParent());
        // Re-export rather than a bare import so the module survives tree shaking. A package that
        // declares itself free of side effects would otherwise be dropped whole, taking its element
        // registration with it. Re-exporting forces Bun to evaluate the module and keep its output.
        Files.writeString(stubFile,
            "export * from " + getJsStringLiteral(entry.getSource()) + ";\n");
        entry.setBuildPath(NPM_ENTRY_DIR + "/" + stubPath);
      } else {
        // The build path is the resolved file relative to the source root, which equals the source
        // for a local file and the extracted location for a file a dependency shipped. The source
        // stays the declared @BundleEntry value the runtime resolves the class by.
        entry.setBuildPath(
            sourceRoot.relativize(entry.getResolvedFile()).toString().replace('\\', '/'));
      }
    }

    return entries;
  }

  private static String getNpmStubPath(String specifier) {
    String lower = specifier.toLowerCase(Locale.ROOT);
    for (String extension : SCRIPT_EXTENSIONS) {
      if (lower.endsWith(extension)) {
        return specifier;
      }
    }

    return specifier + ".js";
  }

  private static String getJsStringLiteral(String value) {
    return "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\"";
  }

  private BundleEntryDeclaration writeEagerEntry(Path generatedDir,
      List<BundleEntryDeclaration> entries) throws IOException {
    StringBuilder module = new StringBuilder();
    for (BundleEntryDeclaration entry : entries) {
      // Each entry's build file sits under the source root. The eager module sits one level deeper,
      // in the generated directory, so it reaches each of them with a leading parent segment.
      module.append("import ").append(getJsStringLiteral("../" + entry.getBuildPath()))
          .append(";\n");
    }

    Files.writeString(generatedDir.resolve(EAGER_ENTRY), module.toString());

    return new BundleEntryDeclaration().setSource(BundleIndex.EAGER_KEY)
        .setBuildPath(NPM_ENTRY_DIR + "/" + EAGER_ENTRY);
  }

  private static void mergeBindings(Map<String, Set<String>> target,
      Map<String, Set<String>> source) {
    for (Map.Entry<String, Set<String>> entry : source.entrySet()) {
      target.computeIfAbsent(entry.getKey(), key -> new LinkedHashSet<>()).addAll(entry.getValue());
    }
  }

  private void runExtensions(List<BundleExtension> extensions, BundleContext context,
      Map<String, Boolean> overrides) throws IOException {
    for (BundleExtension extension : extensions) {
      if (isExtensionActive(extension, context, overrides)) {
        log.info("enabled extension '{}'", extension.getId());
        extension.onWillBundle(context);
      } else {
        log.info("extension '{}' is off, enable it with the plugins configuration",
            extension.getId());
      }
    }
  }

  private Set<String> scanSourceExtensions(Path sourceRoot) {
    if (sourceRoot == null || !Files.isDirectory(sourceRoot)) {
      return Set.of();
    }

    Set<String> extensions = new LinkedHashSet<>();
    try (Stream<Path> walk = Files.walk(sourceRoot)) {
      for (Path path : (Iterable<Path>) walk::iterator) {
        if (!Files.isRegularFile(path) || isInsideNodeModules(sourceRoot, path)) {
          continue;
        }

        String name = path.getFileName().toString();
        int dot = name.lastIndexOf('.');
        if (dot > 0 && dot < name.length() - 1) {
          extensions.add(name.substring(dot + 1).toLowerCase(Locale.ROOT));
        }
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    return extensions;
  }

  private boolean isInsideNodeModules(Path root, Path path) {
    for (Path segment : root.relativize(path)) {
      if (segment.toString().equals("node_modules")) {
        return true;
      }
    }

    return false;
  }

  private static List<BundlePackageDeclaration> mergeDeclarations(
      List<BundlePackageDeclaration> base, List<BundlePackageDeclaration> extra) {
    if (extra.isEmpty()) {
      return base;
    }

    List<BundlePackageDeclaration> all = new ArrayList<>(base);
    Set<String> names = new HashSet<>();
    for (BundlePackageDeclaration declaration : base) {
      names.add(declaration.getName());
    }
    for (BundlePackageDeclaration declaration : extra) {
      if (names.add(declaration.getName())) {
        all.add(declaration);
      }
    }

    return all;
  }

  private void installAtRoot(Request request, List<BundlePackageDeclaration> declarations)
      throws IOException, InterruptedException {
    Path npmRoot = request.getNpmRoot();
    Files.createDirectories(npmRoot);
    Map<String, Object> existing = packageJson.read(npmRoot.resolve(PACKAGE_JSON));
    Map<String, Object> model =
        packageJson.createModel(request.getProjectArtifactId(), declarations, existing);
    Path written = packageJson.write(npmRoot, model);
    log.info("wrote {}", written);

    if (!declarations.isEmpty()) {
      int installCode = bun.execute(npmRoot, List.of("install", "--no-summary"));
      if (installCode != 0) {
        throw new IOException("bun install failed with exit code " + installCode);
      }
    }
  }

  private void prepareOutputDir(Path outputDir) throws IOException {
    if (Files.isDirectory(outputDir)) {
      clearDirectory(outputDir);
    }

    Files.createDirectories(outputDir);
  }

  private static void clearDirectory(Path dir) throws IOException {
    try (Stream<Path> walk = Files.walk(dir)) {
      walk.sorted((a, b) -> b.getNameCount() - a.getNameCount()).filter(p -> !p.equals(dir))
          .forEach(p -> {
            try {
              Files.deleteIfExists(p);
            } catch (IOException e) {
              throw new UncheckedIOException(e);
            }
          });
    } catch (UncheckedIOException e) {
      throw e.getCause();
    }
  }

  private BundleDriverWriter.Config createDriverConfig(Request request,
      List<BundleEntryDeclaration> entries, Path outputDir, Path metafile, List<BunPlugin> plugins,
      boolean production, boolean splitting) {
    String entryNaming = production ? "[dir]/[name]-[hash].[ext]" : "[dir]/[name].[ext]";
    Path userConfig = request.getBundleSourceRoot().resolve(USER_CONFIG_NAME);
    String userConfigPath =
        Files.isRegularFile(userConfig) ? userConfig.toAbsolutePath().toString() : null;

    return new BundleDriverWriter.Config().setEntries(entries)
        .setOutdir(outputDir.toAbsolutePath().toString())
        .setRoot(request.getBundleSourceRoot().toAbsolutePath().toString())
        .setMetafile(metafile.toAbsolutePath().toString()).setEntryNaming(entryNaming)
        .setMinify(production).setHashed(production).setSplitting(splitting).setPlugins(plugins)
        .setUserConfig(userConfigPath);
  }

  private List<String> writeDriverFiles(Request request, Path workDir,
      List<BundleEntryDeclaration> driverEntries, Path outputDir, Path metafile,
      List<BunPlugin> plugins, boolean production, boolean splitting) throws IOException {
    Path scriptPath = driverWriter.writeDriver(workDir);
    driverWriter.writePlugins(workDir, plugins);
    driverWriter.writeConfig(workDir, createDriverConfig(request, driverEntries, outputDir,
        metafile, plugins, production, splitting));

    return List.of("run", scriptPath.toAbsolutePath().toString());
  }

  /**
   * Writes the index to disk and removes the index of the other mode, so a single index file is on
   * the classpath. A built run writes under {@code META-INF}, the development watch writes under
   * the static folder, which the development servers exclude from their reload scan.
   *
   * @param request the parameters for this run
   * @param prepared the prepared build
   * @param development whether to write the development index rather than the built index
   */
  private void writeIndex(Request request, Prepared prepared, boolean development)
      throws IOException {
    Map<String, List<String>> bindings = buildBindings(prepared,
        indexWriter.mapOutputs(prepared.getMetafile(), request.getBundleSourceRoot()));
    String resource =
        development ? BundleIndexDocument.DEVELOPMENT_RESOURCE : BundleIndexDocument.RESOURCE;
    String stale =
        development ? BundleIndexDocument.RESOURCE : BundleIndexDocument.DEVELOPMENT_RESOURCE;
    Path indexPath = indexWriter.write(request.getClassesOutputDir(), bindings, resource);
    Files.deleteIfExists(request.getClassesOutputDir().resolve(stale));
    log.info("wrote {}", indexPath);
  }

  /**
   * Folds the entry outputs into the routed class to output binding the index is written from.
   *
   * <p>
   * Eager mode produced a single output already keyed under the reserved eager marker, so the
   * mapping is the binding as is. Otherwise the entry key to output mapping is folded into a routed
   * class to output binding, and the debug only outputs are added under the reserved debug key.
   * </p>
   *
   * @param prepared the prepared build
   * @param keyToFiles the entry key to output files mapping from the build
   * @return the routed class to output files binding
   */
  private Map<String, List<String>> buildBindings(Prepared prepared,
      Map<String, List<String>> keyToFiles) {
    if (prepared.isEager()) {
      return keyToFiles;
    }

    Map<String, List<String>> bindings =
        indexWriter.bindClasses(keyToFiles, prepared.getBindings());
    addDebugFiles(bindings, keyToFiles, prepared.getDebugSources());

    return bindings;
  }

  /**
   * Adds the output files of the debug only entries under the reserved index key, so the runtime
   * injects them only in debug mode.
   *
   * @param target the routed class to output files binding the index is built from
   * @param keyToFiles the entry key to output files mapping from the build
   * @param debugSources the entry sources declared {@code debug = true}
   */
  static void addDebugFiles(Map<String, List<String>> target, Map<String, List<String>> keyToFiles,
      Set<String> debugSources) {
    if (debugSources.isEmpty()) {
      return;
    }

    List<String> debugFiles = new ArrayList<>();
    for (String source : debugSources) {
      List<String> outputs = keyToFiles.get(source);
      if (outputs == null) {
        continue;
      }

      for (String file : outputs) {
        if (!debugFiles.contains(file)) {
          debugFiles.add(file);
        }
      }
    }

    if (!debugFiles.isEmpty()) {
      target.put(BundleIndex.DEBUG_KEY, debugFiles);
    }
  }

  private List<String> syncQuietly(Path from, Path to) {
    try {
      List<String> changed = syncChangedFiles(from, to);
      if (!changed.isEmpty()) {
        log.info("synced {} changed bundle file(s)", changed.size());
      }

      return changed;
    } catch (IOException e) {
      log.warn("failed to sync watch output: {}", e.getMessage());

      return List.of();
    }
  }

  static List<String> syncChangedFiles(Path from, Path to) throws IOException {
    if (!Files.isDirectory(from)) {
      return List.of();
    }

    Files.createDirectories(to);
    Set<String> staged = new HashSet<>();
    List<String> changed = new ArrayList<>();

    try (Stream<Path> walk = Files.walk(from)) {
      for (Path src : (Iterable<Path>) walk::iterator) {
        if (!Files.isRegularFile(src)) {
          continue;
        }

        String relative = from.relativize(src).toString().replace('\\', '/');
        staged.add(relative);
        Path dst = to.resolve(relative);

        if (!Files.exists(dst) || !isSameContent(src, dst)) {
          Files.createDirectories(dst.getParent());
          Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING);
          changed.add(relative);
        }
      }
    }

    List<Path> stale = new ArrayList<>();
    try (Stream<Path> walk = Files.walk(to)) {
      for (Path dst : (Iterable<Path>) walk::iterator) {
        String relative = to.relativize(dst).toString().replace('\\', '/');
        if (Files.isRegularFile(dst) && !staged.contains(relative)) {
          stale.add(dst);
        }
      }
    }

    for (Path dst : stale) {
      Files.deleteIfExists(dst);
      changed.add(to.relativize(dst).toString().replace('\\', '/'));
    }

    return changed;
  }

  private static boolean isSameContent(Path a, Path b) throws IOException {
    if (Files.size(a) != Files.size(b)) {
      return false;
    }

    return Arrays.equals(Files.readAllBytes(a), Files.readAllBytes(b));
  }

  private static boolean isRebuildComplete(String line) {
    return line.contains("Bundled ");
  }

  private void installDependencies(Request request) throws IOException, InterruptedException {
    ClasspathPackageScanner.Result scan =
        scanner.scan(getUris(request.getClasspathRoots()), request.getExcludedPackages());
    installAtRoot(request, scan.getPackages());
  }

  private static boolean hasTestFiles(Path sourceRoot) throws IOException {
    try (Stream<Path> walk = Files.walk(sourceRoot)) {
      return walk.filter(Files::isRegularFile)
          .map(p -> p.getFileName().toString().toLowerCase(Locale.ROOT))
          .anyMatch(BundlerExecution::isTestFile);
    }
  }

  private static boolean isTestFile(String name) {
    int dot = name.indexOf('.');
    if (dot < 0) {
      return false;
    }

    String stem = name.substring(0, name.lastIndexOf('.'));

    return stem.endsWith(".test") || stem.endsWith(".spec") || stem.endsWith("_test")
        || stem.endsWith("_spec");
  }

  private static final class CompileContext { // NOSONAR

    private final Request request;
    private final List<BundleExtension> extensions;
    private final Set<String> sourceExtensions;
    private final boolean production;

    private CompileContext(Request request, List<BundleExtension> extensions,
        Set<String> sourceExtensions, boolean production) {
      this.request = request;
      this.extensions = extensions;
      this.sourceExtensions = sourceExtensions;
      this.production = production;
    }

    private Request getRequest() {
      return request;
    }

    private List<BundleExtension> getExtensions() {
      return extensions;
    }

    private Set<String> getSourceExtensions() {
      return sourceExtensions;
    }

    private boolean isProduction() {
      return production;
    }
  }

  private static class Prepared {

    private Path metafile = null;
    private List<String> runArgs = null;
    private Map<String, Set<String>> bindings = Map.of();
    private Set<String> debugSources = Set.of();
    private boolean eager = false;

    /**
     * Sets the metafile path.
     *
     * @param metafile the metafile path
     * @return the prepared run
     */
    private Prepared setMetafile(Path metafile) {
      this.metafile = metafile;

      return this;
    }

    /**
     * Gets the metafile path.
     *
     * @return the metafile path
     * @see #setMetafile(Path)
     */
    private Path getMetafile() {
      return metafile;
    }

    /**
     * Sets the run arguments passed to Bun.
     *
     * @param runArgs the run arguments
     * @return the prepared run
     */
    private Prepared setRunArgs(List<String> runArgs) {
      this.runArgs = runArgs;

      return this;
    }

    /**
     * Gets the run arguments passed to Bun.
     *
     * @return the run arguments
     * @see #setRunArgs(List)
     */
    private List<String> getRunArgs() {
      return runArgs;
    }

    /**
     * Sets the routed class to entry key binding the runtime resolves a class by.
     *
     * @param bindings the routed class to entry key binding
     * @return the prepared run
     */
    private Prepared setBindings(Map<String, Set<String>> bindings) {
      this.bindings = bindings;

      return this;
    }

    /**
     * Gets the routed class to entry key binding the runtime resolves a class by.
     *
     * @return the routed class to entry key binding
     * @see #setBindings(Map)
     */
    private Map<String, Set<String>> getBindings() {
      return bindings;
    }

    /**
     * Sets the entry sources declared debug only, injected by the runtime only in debug mode.
     *
     * @param debugSources the debug only entry sources
     * @return the prepared run
     */
    private Prepared setDebugSources(Set<String> debugSources) {
      this.debugSources = debugSources;

      return this;
    }

    /**
     * Gets the entry sources declared debug only, injected by the runtime only in debug mode.
     *
     * @return the debug only entry sources
     * @see #setDebugSources(Set)
     */
    private Set<String> getDebugSources() {
      return debugSources;
    }

    /**
     * Sets whether the run produced a single eager bundle.
     *
     * @param eager {@code true} for a single eager bundle
     * @return the prepared run
     */
    private Prepared setEager(boolean eager) {
      this.eager = eager;

      return this;
    }

    /**
     * Indicates whether the run produced a single eager bundle.
     *
     * @return {@code true} for a single eager bundle
     * @see #setEager(boolean)
     */
    private boolean isEager() {
      return eager;
    }
  }

  /**
   * The inputs needed to run a bundler invocation.
   */
  public static class Request {

    private String projectArtifactId = null;
    private List<File> classpathRoots = null;
    private Path bundleSourceRoot = null;
    private Path workDir = null;
    private Path classesOutputDir = null;
    private Path npmRoot = null;
    private List<Path> sourceScanRoots = List.of();
    private Map<String, Boolean> extensionOverrides = Map.of();
    private Set<String> excludedPackages = Set.of();
    private boolean eager = false;
    private List<String> testArgs = List.of();

    /**
     * Sets the host Maven artifactId, used as the package.json name and the output namespace.
     *
     * @param projectArtifactId the host Maven artifactId
     * @return the request
     */
    public Request setProjectArtifactId(String projectArtifactId) {
      this.projectArtifactId = projectArtifactId;

      return this;
    }

    /**
     * Gets the host Maven artifactId, used as the package.json name and the output namespace.
     *
     * @return the host Maven artifactId
     * @see #setProjectArtifactId(String)
     */
    public String getProjectArtifactId() {
      return projectArtifactId;
    }

    /**
     * Sets the compile classpath roots to scan for annotations.
     *
     * @param classpathRoots the compile classpath roots
     * @return the request
     */
    public Request setClasspathRoots(List<File> classpathRoots) {
      this.classpathRoots = classpathRoots;

      return this;
    }

    /**
     * Gets the compile classpath roots to scan for annotations.
     *
     * @return the compile classpath roots
     * @see #setClasspathRoots(List)
     */
    public List<File> getClasspathRoots() {
      return classpathRoots;
    }

    /**
     * Sets the source root that contains entry files.
     *
     * @param bundleSourceRoot the bundle source root
     * @return the request
     */
    public Request setBundleSourceRoot(Path bundleSourceRoot) {
      this.bundleSourceRoot = bundleSourceRoot;

      return this;
    }

    /**
     * Gets the source root that contains entry files.
     *
     * @return the bundle source root
     * @see #setBundleSourceRoot(Path)
     */
    public Path getBundleSourceRoot() {
      return bundleSourceRoot;
    }

    /**
     * Sets the directory used for package.json and node_modules.
     *
     * @param workDir the working directory
     * @return the request
     */
    public Request setWorkDir(Path workDir) {
      this.workDir = workDir;

      return this;
    }

    /**
     * Gets the directory used for package.json and node_modules.
     *
     * @return the working directory
     * @see #setWorkDir(Path)
     */
    public Path getWorkDir() {
      return workDir;
    }

    /**
     * Sets the project classes output folder.
     *
     * @param classesOutputDir the project classes output folder
     * @return the request
     */
    public Request setClassesOutputDir(Path classesOutputDir) {
      this.classesOutputDir = classesOutputDir;

      return this;
    }

    /**
     * Gets the project classes output folder.
     *
     * @return the project classes output folder
     * @see #setClassesOutputDir(Path)
     */
    public Path getClassesOutputDir() {
      return classesOutputDir;
    }

    /**
     * Sets the directory the package.json and node_modules are installed into, the project root, so
     * the bundle sources resolve packages by walking up to it.
     *
     * @param npmRoot the npm root directory
     * @return the request
     */
    public Request setNpmRoot(Path npmRoot) {
      this.npmRoot = npmRoot;

      return this;
    }

    /**
     * Gets the directory the package.json and node_modules are installed into.
     *
     * @return the npm root directory
     * @see #setNpmRoot(Path)
     */
    public Path getNpmRoot() {
      return npmRoot;
    }

    /**
     * Sets the project source roots a source scanning plugin points its scan at, the application
     * sources rather than the bundle frontend, defending against a null list.
     *
     * @param sourceScanRoots the project source roots, may be null
     * @return the request
     */
    public Request setSourceScanRoots(List<Path> sourceScanRoots) {
      this.sourceScanRoots = sourceScanRoots == null ? List.of() : List.copyOf(sourceScanRoots);

      return this;
    }

    /**
     * Gets the project source roots a source scanning plugin points its scan at, never null.
     *
     * @return the project source roots
     * @see #setSourceScanRoots(List)
     */
    public List<Path> getSourceScanRoots() {
      return sourceScanRoots;
    }

    /**
     * Sets the per id extension enablement overrides, defending against a null map.
     *
     * @param extensionOverrides the per id enablement overrides, may be null
     * @return the request
     */
    public Request setExtensionOverrides(Map<String, Boolean> extensionOverrides) {
      this.extensionOverrides =
          extensionOverrides == null ? Map.of() : Map.copyOf(extensionOverrides);

      return this;
    }

    /**
     * Gets the per id extension enablement overrides, never null.
     *
     * @return the per id enablement overrides
     * @see #setExtensionOverrides(Map)
     */
    public Map<String, Boolean> getExtensionOverrides() {
      return extensionOverrides;
    }

    /**
     * Sets the extra package prefixes to skip during the annotation scan, on top of the built in
     * exclusions.
     *
     * @param excludedPackages the extra package prefixes to skip, may be null
     * @return the request
     */
    public Request setExcludedPackages(Collection<String> excludedPackages) {
      this.excludedPackages = excludedPackages == null ? Set.of() : Set.copyOf(excludedPackages);

      return this;
    }

    /**
     * Gets the extra package prefixes to skip during the annotation scan, never null.
     *
     * @return the extra package prefixes to skip
     * @see #setExcludedPackages(Set)
     */
    public Set<String> getExcludedPackages() {
      return excludedPackages;
    }

    /**
     * Sets whether to build a single eager bundle the runtime loads at app start, required where
     * the runtime serves no static folder and assets are inlined rather than served.
     *
     * @param eager {@code true} to build a single eager bundle
     * @return the request
     */
    public Request setEager(boolean eager) {
      this.eager = eager;

      return this;
    }

    /**
     * Indicates whether to build a single eager bundle rather than per component output.
     *
     * @return {@code true} to build a single eager bundle
     * @see #setEager(boolean)
     */
    public boolean isEager() {
      return eager;
    }

    /**
     * Sets the extra arguments appended to the {@code bun test} invocation, defending against a
     * null list.
     *
     * @param testArgs the extra test arguments, may be null
     * @return the request
     */
    public Request setTestArgs(Collection<String> testArgs) {
      this.testArgs = testArgs == null ? List.of() : List.copyOf(testArgs);

      return this;
    }

    /**
     * Gets the extra arguments appended to the {@code bun test} invocation, never null.
     *
     * @return the extra test arguments
     * @see #setTestArgs(Collection)
     */
    public List<String> getTestArgs() {
      return testArgs;
    }
  }

}
