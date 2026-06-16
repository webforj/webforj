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
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
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
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Stream;

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
  private BundleLogger log = BundleLogger.system();

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
    return run(request, BundleLogger.system());
  }

  /**
   * Runs a single shot production bundle with hashed, minified output, reporting through the given
   * logger.
   *
   * @param request the parameters for this run
   * @param logger where the output is reported, the invoker owns it
   *
   * @return the served output directory, or {@code null} when nothing was bundled
   * @throws IOException on any IO failure
   * @throws InterruptedException if the run is interrupted
   */
  public Path run(Request request, BundleLogger logger) throws IOException, InterruptedException {
    useLogger(logger);

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
   * @return the running watch, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public WatchSession watch(Request request) throws IOException, InterruptedException {
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
   * @return the running watch, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public WatchSession watch(Request request, Consumer<List<String>> rebuildListener)
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
   * @param logger where the watch output is reported, a watch passes one that forwards to the
   *        running application, may be null
   *
   * @return the running watch, or {@code null} when there was nothing to watch
   * @throws IOException on any IO failure
   * @throws InterruptedException if the initial build is interrupted
   */
  public WatchSession watch(Request request, Consumer<List<String>> rebuildListener,
      BundleLogger logger) throws IOException, InterruptedException {
    // During a watch every bundler line is forwarded to the running application, so nothing is
    // written to the build console while the application is up.
    useLogger(logger);
    CompileContext compileContext = createCompileContext(request, false);
    Path servedDir = getServedOutputDir(request);
    Path stagingDir = request.getWorkDir().resolve(WATCH_STAGING_DIR);

    Prepared prepared = buildWatch(compileContext, servedDir, stagingDir);
    if (prepared == null) {
      return null;
    }

    // A development restart can change the set of entries the Bun watcher builds, so the session
    // rescans on every restart and rebuilds only when the set actually changed. Each rebuild builds
    // a fresh context so a source extension introduced at watch time, such as the first scss file,
    // is picked up by the rescan that the fresh context runs.
    Process watcher =
        startWatcher(compileContext, servedDir, stagingDir, prepared, rebuildListener, false);

    AtomicReference<Prepared> current = new AtomicReference<>(prepared);

    WatchSession.WatchHost host = new WatchSession.WatchHost() {
      @Override
      public BundleEntrySet currentEntrySet() {
        return scanEntrySet(request);
      }

      @Override
      public void syncOutput() throws IOException {
        Prepared prepared = current.get();
        if (prepared == null) {
          clearOutput(request, servedDir);
        } else {
          BundlerExecution.this.syncOutput(request, servedDir, stagingDir, prepared);
        }
      }

      @Override
      public Process restartWatcher() throws IOException, InterruptedException {
        CompileContext context = createCompileContext(request, false);
        Prepared rebuilt = buildWatch(context, servedDir, stagingDir);
        current.set(rebuilt);
        if (rebuilt == null) {
          return null;
        }

        return startWatcher(context, servedDir, stagingDir, rebuilt, rebuildListener, true);
      }
    };

    return new WatchSession(watcher, host, log);
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
    return test(request, BundleLogger.system());
  }

  /**
   * Runs the Bun test runner over the bundle source root, reporting through the given logger.
   *
   * @param request the parameters for this run
   * @param logger where the output is reported, the invoker owns it
   *
   * @return the Bun test exit code, zero when there is nothing to test
   * @throws IOException on any IO failure
   * @throws InterruptedException if the run is interrupted
   */
  public int test(Request request, BundleLogger logger) throws IOException, InterruptedException {
    useLogger(logger);

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

    return bun.execute(sourceRoot, args, line -> log.info("{}", line));
  }

  private void useLogger(BundleLogger logger) {
    this.log = logger != null ? logger : BundleLogger.system();
    bun.setLogger(this.log);
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

  private static Set<String> scanSourceExtensions(Path sourceRoot) {
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

  private static boolean isInsideNodeModules(Path root, Path path) {
    for (Path segment : root.relativize(path)) {
      if (segment.toString().equals("node_modules")) {
        return true;
      }
    }

    return false;
  }

  private static Path getServedOutputDir(Request request) {
    return request.getClassesOutputDir().resolve(OUTPUT_RESOURCE_PREFIX);
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

    clearGeneratedDir(generatedDir);
    extractDependencyFrontend(classpath, generatedDir);

    List<BundleEntryDeclaration> entries =
        new ArrayList<>(prepareBaseEntries(resolveEntries(scan, sourceRoot, generatedDir),
            generatedDir, sourceRoot));

    BundleContext context = createContext(compileContext, outputDir, false);
    notifyWillBundle(compileContext.getExtensions(), context, request.getExtensionOverrides());

    entries.addAll(context.getEntries());

    final List<BunPlugin> plugins = context.getPlugins();
    if (entries.isEmpty()) {
      log.info("nothing to bundle, skipping Bun invocation");
      clearGeneratedDir(generatedDir);

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

    writeGeneratedGitignore(generatedDir);

    Path workDir = request.getWorkDir();
    Files.createDirectories(workDir);
    List<BundlePackageDeclaration> declarations =
        mergeDeclarations(scan.getPackages(), context.getPackages());
    installAtRoot(request, declarations);

    prepareOutputDir(outputDir);

    Path metafile = workDir.resolve(META_FILE);
    BundleDriverWriter.Config config = createDriverConfig(request, driverEntries, outputDir,
        metafile, plugins, compileContext.isProduction(), !eager, context.getWatchPaths());
    List<String> runArgs = writeDriverFiles(workDir, plugins, config);

    return new Prepared().setMetafile(metafile).setRunArgs(runArgs).setBindings(bindings)
        .setDebugSources(debugSources).setEager(eager);
  }

  private static Set<URI> getUris(List<File> roots) {
    Set<URI> uris = new LinkedHashSet<>();
    for (File root : roots) {
      if (root == null || !root.exists()) {
        continue;
      }

      uris.add(root.toURI());
    }

    return uris;
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

  private static void clearGeneratedDir(Path generatedDir) throws IOException {
    // Remove a previous run's generated sources. This only deletes, it never creates, so a run that
    // generates nothing leaves no directory behind.
    if (Files.isDirectory(generatedDir)) {
      clearDirectory(generatedDir);
      Files.deleteIfExists(generatedDir);
    }
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

  private void extractDependencyFrontend(Set<URI> classpath, Path generatedDir) throws IOException {
    // A dependency can ship its own frontend inside its JAR. Extract it into the generated sources
    // directory before resolving entries, so the single build compiles it the same as local source.
    int extracted = new DependencyEntryExtractor().extract(classpath, generatedDir);
    if (extracted > 0) {
      log.info("extracted {} frontend file(s) shipped by dependencies", extracted);
    }
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

  private static List<BundleEntryDeclaration> prepareBaseEntries(
      List<BundleEntryDeclaration> entries, Path generatedDir, Path sourceRoot) throws IOException {
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

  private void notifyWillBundle(List<BundleExtension> extensions, BundleContext context,
      Map<String, Boolean> overrides) throws IOException {
    for (BundleExtension extension : extensions) {
      if (isExtensionActive(extension, context, overrides)) {
        log.info("enabled extension '{}'", extension.getId());
        extension.onWillBundle(context);
      }
    }
  }

  private static boolean isExtensionActive(BundleExtension extension, BundleContext context,
      Map<String, Boolean> overrides) {
    Boolean override = overrides.get(extension.getId());

    return override != null ? override : extension.isEnabledByDefault(context);
  }

  private static BundleEntryDeclaration writeEagerEntry(Path generatedDir,
      List<BundleEntryDeclaration> entries) throws IOException {
    StringBuilder module = new StringBuilder();
    for (BundleEntryDeclaration entry : entries) {
      // Each entry's build file sits under the source root. The eager module sits one level deeper,
      // in the generated directory, so it reaches each of them with a leading parent segment.
      module.append("import ").append(getJsStringLiteral("../" + entry.getBuildPath()))
          .append(";\n");
    }

    Files.createDirectories(generatedDir);
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

  private static void writeGeneratedGitignore(Path generatedDir) throws IOException {
    // The directory exists only when this run wrote a generated source into it, so keep whatever it
    // holds out of version control. When nothing needed generating the directory was never created,
    // and there is nothing to ignore or to clean up.
    if (Files.isDirectory(generatedDir)) {
      Files.writeString(generatedDir.resolve(".gitignore"), "*\n");
    }
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
    Path packageJsonFile = npmRoot.resolve(PACKAGE_JSON);

    // Nothing declares an npm dependency and the project ships no package.json of its own. There is
    // nothing to install and no manifest to maintain, so the project tree is left untouched.
    if (declarations.isEmpty() && !Files.isRegularFile(packageJsonFile)) {
      return;
    }

    Files.createDirectories(npmRoot);
    Map<String, Object> existing = packageJson.read(packageJsonFile);
    Map<String, Object> model =
        packageJson.createModel(request.getProjectName(), declarations, existing);
    Path written = packageJson.write(npmRoot, model);
    log.info("wrote {}", written);

    if (!declarations.isEmpty()) {
      int installCode = bun.execute(npmRoot, List.of("install", "--no-summary"));
      if (installCode != 0) {
        throw new IOException("bun install failed with exit code " + installCode);
      }
    }
  }

  private static void prepareOutputDir(Path outputDir) throws IOException {
    if (Files.isDirectory(outputDir)) {
      clearDirectory(outputDir);
    }

    Files.createDirectories(outputDir);
  }

  private static BundleDriverWriter.Config createDriverConfig(Request request,
      List<BundleEntryDeclaration> entries, Path outputDir, Path metafile, List<BunPlugin> plugins,
      boolean production, boolean splitting, Set<Path> watchPaths) {
    String entryNaming = production ? "[dir]/[name]-[hash].[ext]" : "[dir]/[name].[ext]";
    Path userConfig = request.getBundleSourceRoot().resolve(USER_CONFIG_NAME);
    String userConfigPath =
        Files.isRegularFile(userConfig) ? userConfig.toAbsolutePath().toString() : null;

    List<String> watchDirs = new ArrayList<>();
    for (Path path : watchPaths) {
      if (Files.isDirectory(path)) {
        watchDirs.add(path.toAbsolutePath().toString());
      }
    }

    return new BundleDriverWriter.Config().setEntries(entries)
        .setOutdir(outputDir.toAbsolutePath().toString())
        .setRoot(request.getBundleSourceRoot().toAbsolutePath().toString())
        .setMetafile(metafile.toAbsolutePath().toString()).setEntryNaming(entryNaming)
        .setMinify(production).setHashed(production).setSplitting(splitting).setPlugins(plugins)
        .setUserConfig(userConfigPath).setWatchPaths(watchDirs);
  }

  private List<String> writeDriverFiles(Path workDir, List<BunPlugin> plugins,
      BundleDriverWriter.Config config) throws IOException {
    Path scriptPath = driverWriter.writeDriver(workDir);
    driverWriter.writePlugins(workDir, plugins);
    driverWriter.writeConfig(workDir, config);

    return List.of("run", scriptPath.toAbsolutePath().toString());
  }

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

  private Prepared buildWatch(CompileContext compileContext, Path servedDir, Path stagingDir)
      throws IOException, InterruptedException {
    Prepared prepared = compile(compileContext, stagingDir);
    if (prepared == null) {
      // Nothing resolved to build, so clear the served output and the index, so a removed entry
      // stops being served instead of lingering as stale content from the last build.
      clearOutput(compileContext.getRequest(), servedDir);

      return null;
    }

    syncOutput(compileContext.getRequest(), servedDir, stagingDir, prepared);
    notifyDidBundle(compileContext, servedDir, false);

    return prepared;
  }

  private Process startWatcher(CompileContext compileContext, Path servedDir, Path stagingDir,
      Prepared prepared, Consumer<List<String>> rebuildListener, boolean reloadOnBaseline)
      throws IOException, InterruptedException {
    List<String> watchArgs = new ArrayList<>(prepared.getRunArgs());
    watchArgs.add("--watch");
    log.info("starting bun watch, edit sources to rebuild");

    AtomicBoolean baselineEstablished = new AtomicBoolean(false);

    return bun.start(compileContext.getRequest().getBundleSourceRoot(), watchArgs, line -> {
      log.info("{}", line);

      if (isRebuildComplete(line)) {
        // The watcher reemits the whole graph to its staging tree on every rebuild, including when
        // a
        // source file is added or removed, so sync the served output from what was built. The watch
        // thread must survive a transient filesystem race while bun rewrites staging under it, so a
        // failure here is logged and left for the next rebuild to reconcile.
        List<String> changed;
        try {
          changed = syncOutput(compileContext.getRequest(), servedDir, stagingDir, prepared);
        } catch (IOException | RuntimeException e) {
          log.warn("failed to sync watch output after a rebuild: {}", e.getMessage());

          return;
        }

        if (!changed.isEmpty()) {
          log.info("synced {} changed bundle file(s)", changed.size());
          notifyDidBundle(compileContext, servedDir, true);
        }

        // The watcher emits its own build when it starts. On the initial start that baseline only
        // syncs the served directory and never reloads the browser. After an entry change the
        // baseline is the new bundle, so it must reach the reload listener.
        if (baselineEstablished.compareAndSet(false, true) && !reloadOnBaseline) {
          return;
        }

        if (rebuildListener != null && !changed.isEmpty()) {
          rebuildListener.accept(changed);
        }
      }
    });
  }

  private static boolean isRebuildComplete(String line) {
    return line.contains("Bundled ");
  }

  private BundleEntrySet scanEntrySet(Request request) {
    ClasspathPackageScanner.Result scan =
        scanner.scan(getUris(request.getClasspathRoots()), request.getExcludedPackages());

    return BundleEntrySet.from(scan);
  }

  private List<String> syncOutput(Request request, Path servedDir, Path stagingDir,
      Prepared prepared) throws IOException {
    Files.createDirectories(servedDir);
    List<String> changed = syncChangedFiles(stagingDir, servedDir);
    writeIndex(request, prepared, true);

    return changed;
  }

  private void clearOutput(Request request, Path servedDir) throws IOException {
    if (Files.isDirectory(servedDir)) {
      clearDirectory(servedDir);
    }

    Path index = indexWriter.write(request.getClassesOutputDir(), Map.of(),
        BundleIndexDocument.DEVELOPMENT_RESOURCE);
    log.info("wrote {}", index);
  }

  static List<String> syncChangedFiles(Path from, Path to) throws IOException {
    if (!Files.isDirectory(from)) {
      return List.of();
    }

    Files.createDirectories(to);
    Set<String> staged = new HashSet<>();
    List<String> changed = new ArrayList<>();

    // The watch rebuild rewrites the staging tree while this sync reads it, so files and
    // directories can vanish mid walk. the visitor skips whatever disappears instead of
    // aborting the whole sync.
    Files.walkFileTree(from, new SimpleFileVisitor<Path>() {
      @Override
      public FileVisitResult visitFile(Path src, BasicFileAttributes attrs) {
        if (!attrs.isRegularFile()) {
          return FileVisitResult.CONTINUE;
        }

        String relative = from.relativize(src).toString().replace('\\', '/');
        staged.add(relative);
        Path dst = to.resolve(relative);

        try {
          if (!Files.exists(dst) || !isSameContent(src, dst)) {
            Files.createDirectories(dst.getParent());
            Files.copy(src, dst, StandardCopyOption.REPLACE_EXISTING);
            changed.add(relative);
          }
        } catch (IOException e) {
          // The source changed under the copy, keep the last good output and let the next
          // rebuild reconcile it.
        }

        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult visitFileFailed(Path file, IOException exc) {
        return FileVisitResult.CONTINUE;
      }

      @Override
      public FileVisitResult postVisitDirectory(Path dir, IOException exc) {
        return FileVisitResult.CONTINUE;
      }
    });

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

  private void installDependencies(Request request) throws IOException, InterruptedException {
    ClasspathPackageScanner.Result scan =
        scanner.scan(getUris(request.getClasspathRoots()), request.getExcludedPackages());
    installAtRoot(request, scan.getPackages());
  }

  private static final class CompileContext {

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

    private Prepared setMetafile(Path metafile) {
      this.metafile = metafile;

      return this;
    }

    private Path getMetafile() {
      return metafile;
    }

    private Prepared setRunArgs(List<String> runArgs) {
      this.runArgs = runArgs;

      return this;
    }

    private List<String> getRunArgs() {
      return runArgs;
    }

    private Prepared setBindings(Map<String, Set<String>> bindings) {
      this.bindings = bindings;

      return this;
    }

    private Map<String, Set<String>> getBindings() {
      return bindings;
    }

    private Prepared setDebugSources(Set<String> debugSources) {
      this.debugSources = debugSources;

      return this;
    }

    private Set<String> getDebugSources() {
      return debugSources;
    }

    private Prepared setEager(boolean eager) {
      this.eager = eager;

      return this;
    }

    private boolean isEager() {
      return eager;
    }
  }

  /**
   * The inputs needed to run a bundler invocation.
   */
  public static class Request {

    private String projectName = null;
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
     * Sets the project name, used as the package.json name and the output namespace.
     *
     * @param projectName the project name, used as the generated package.json name
     * @return the request
     */
    public Request setProjectName(String projectName) {
      this.projectName = projectName;

      return this;
    }

    /**
     * Gets the host Maven artifactId, used as the package.json name and the output namespace.
     *
     * @return the host Maven artifactId
     * @see #setProjectName(String)
     */
    public String getProjectName() {
      return projectName;
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
