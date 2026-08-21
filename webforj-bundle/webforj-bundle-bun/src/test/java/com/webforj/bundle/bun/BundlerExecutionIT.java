package com.webforj.bundle.bun;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.webforj.bundle.BundleIndexDocument;
import com.webforj.bundle.annotation.BundleEntry;
import com.webforj.bundle.bun.discovery.BundleEntryResolver;
import com.webforj.bundle.bun.discovery.ClasspathPackageScanner;
import com.webforj.bundle.bun.it.fixture.CardView;
import com.webforj.bundle.bun.it.fixture.CssView;
import com.webforj.bundle.bun.it.fixture.LessView;
import com.webforj.bundle.bun.it.fixture.NoteView;
import com.webforj.bundle.bun.it.fixture.ScssView;
import com.webforj.bundle.bun.runtime.BunRuntime;
import com.webforj.bundle.bun.writer.BundleDriverWriter;
import com.webforj.bundle.bun.writer.BundleIndexWriter;
import com.webforj.bundle.bun.writer.PackageJsonWriter;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundlerExecutionIT {

  private static final String FIXTURE = "bundler-it";
  private static final String TAILWIND_ID = "webforj-tailwind";
  private static final Duration WATCH_TIMEOUT = Duration.ofSeconds(90);

  private static Path fixtureRoot;

  @BeforeAll
  static void locateFixture() throws URISyntaxException {
    fixtureRoot = Path.of(BundlerExecutionIT.class.getClassLoader().getResource(FIXTURE).toURI());
  }

  @Test
  void shouldResolveTheRequestedBunVersion(@TempDir Path dir) throws Exception {
    BunRuntime runtime = createRuntime();
    Path binary = runtime.resolve();

    List<String> lines = new CopyOnWriteArrayList<>();
    int code = runtime.execute(dir, List.of("--version"), lines::add);

    assertTrue(Files.isExecutable(binary), "resolved bun binary must be executable: " + binary);
    assertEquals(0, code, "bun --version must succeed");
    assertEquals(List.of(getVersion()), lines, "bun must report the requested version");
  }

  @Test
  void shouldBundleEveryOfficialExtensionInProduction(@TempDir Path dir) throws Exception {
    Project project = Project.create(dir);
    RecordingLogger log = new RecordingLogger();

    Path served = createExecution().run(project.request(), log);

    assertNotNull(served, "a production run with entries must produce a served directory");
    assertEquals(List.of(), log.warnings, "the production bundle must not warn");
    assertTrue(log.lines.stream().noneMatch(line -> line.startsWith("dropping @BundleEntry")),
        "the production bundle must build every declared entry");

    Map<String, List<String>> bindings = readIndex(project.classes);
    assertBinding(bindings, served, CardView.class, ".js", "it-card");
    assertBinding(bindings, served, NoteView.class, ".js", "bundler-it-note-text");
    assertBinding(bindings, served, ScssView.class, ".css", ".it-scss .it-nested");
    assertBinding(bindings, served, LessView.class, ".css", "border-width:6px");
    assertBinding(bindings, served, CssView.class, ".css", ".it-css");

    String css = readAll(served, ".css");
    assertTrue(css.contains(".p-4"), "tailwind must generate the utilities the sources use");
    assertTrue(css.contains(".text-red-500"),
        "tailwind must generate the utilities the sources use");
  }

  @Test
  void shouldInstallCuratedPackagesAndKeepTheLockfileFormat(@TempDir Path dir) throws Exception {
    Project project = Project.create(dir);

    createExecution().run(project.request(), new RecordingLogger());

    String packageJson = Files.readString(project.root.resolve("package.json"));
    for (String name : List.of("\"lit\"", "\"sass\"", "\"less\"", "\"tailwindcss\"",
        "\"bun-plugin-tailwind\"")) {
      assertTrue(packageJson.contains(name), "package.json must declare " + name);
    }

    Path lock = project.root.resolve("bun.lock");
    assertTrue(Files.isRegularFile(lock), "bun install must write bun.lock");
    assertTrue(Files.readString(lock).contains("\"lockfileVersion\""),
        "bun.lock must be the text lockfile");
    assertTrue(Files.isDirectory(project.root.resolve("node_modules").resolve("lit")),
        "bun install must install the declared npm package");
  }

  @Test
  void shouldRebuildOnSourceEditDuringWatchAndStopWithTheSession(@TempDir Path dir)
      throws Exception {
    Project project = Project.create(dir);
    CountDownLatch rebuilt = new CountDownLatch(1);
    List<String> changed = new CopyOnWriteArrayList<>();
    Path servedScss = project.classes.resolve("static/frontend/styles/panel.css");
    RecordingLogger log = new RecordingLogger();
    ProcessHandle watcher;

    try (WatchSession session = createExecution().watch(project.request(), files -> {
      changed.addAll(files);
      rebuilt.countDown();
    }, log)) {
      assertNotNull(session, "a watch with entries must start a session");
      assertTrue(Files.readString(servedScss).contains(".it-scss .it-nested"),
          "the watch must serve the initial build");
      watcher = findBunChild().orElse(null);
      assertNotNull(watcher, "the watch must keep a bun child process");
      assertTrue(log.awaitLines("Bundled ", 2, WATCH_TIMEOUT),
          "the watcher must report its baseline build before sources are edited");

      Files.writeString(project.frontend.resolve("styles/panel.scss"),
          "\n.it-watch { color: #654321; }\n", StandardOpenOption.APPEND);

      assertTrue(rebuilt.await(WATCH_TIMEOUT.toSeconds(), TimeUnit.SECONDS),
          "editing a source must trigger a rebuild within " + WATCH_TIMEOUT);
      assertTrue(changed.stream().anyMatch(f -> f.endsWith("panel.css")),
          "the rebuild must report the changed stylesheet, got " + changed);
      assertTrue(Files.readString(servedScss).contains(".it-watch"),
          "the served stylesheet must carry the edit");
    }

    assertDoesNotThrow(() -> watcher.onExit().get(10, TimeUnit.SECONDS),
        "closing the session must end the bun child process");
  }

  private static void assertBinding(Map<String, List<String>> bindings, Path served, Class<?> owner,
      String extension, String expected) throws IOException {
    List<String> files = bindings.get(owner.getName());
    assertNotNull(files, owner.getSimpleName() + " must be bound in the index");
    Optional<String> file = files.stream().filter(f -> f.endsWith(extension)).findFirst();
    assertTrue(file.isPresent(),
        owner.getSimpleName() + " must map to a " + extension + " output, got " + files);
    assertTrue(Files.readString(served.resolve(file.get())).contains(expected),
        owner.getSimpleName() + " output must contain '" + expected + "'");
  }

  private static Map<String, List<String>> readIndex(Path classes) throws IOException {
    Path index = classes.resolve(BundleIndexDocument.RESOURCE);
    assertTrue(Files.isRegularFile(index), "production run must write " + index);
    try (Reader reader = Files.newBufferedReader(index, StandardCharsets.UTF_8)) {
      return new Gson().fromJson(reader, BundleIndexDocument.class).toIndex().getBindings();
    }
  }

  private static String readAll(Path root, String extension) throws IOException {
    StringBuilder all = new StringBuilder();
    try (Stream<Path> files = Files.walk(root)) {
      for (Path file : files.filter(f -> f.toString().endsWith(extension)).toList()) {
        all.append(Files.readString(file)).append('\n');
      }
    }

    return all.toString();
  }

  private static Optional<ProcessHandle> findBunChild() {
    return ProcessHandle.current().children()
        .filter(p -> p.info().command().orElse("").contains("bun")).findFirst();
  }

  private static String getVersion() {
    return System.getProperty("webforj.bundler.version", BunRuntime.DEFAULT_VERSION);
  }

  private static BunRuntime createRuntime() {
    String cache = System.getProperty("webforj.bundler.cacheDir",
        Path.of(System.getProperty("user.home"), ".webforj", "bun").toString());

    return BunRuntime.create().setCacheRoot(Path.of(cache)).setVersion(getVersion()).build();
  }

  private static BundlerExecution createExecution() {
    return BundlerExecution.create().setScanner(new ClasspathPackageScanner())
        .setResolver(new BundleEntryResolver()).setPackageJsonWriter(new PackageJsonWriter())
        .setIndexWriter(new BundleIndexWriter()).setDriverWriter(new BundleDriverWriter())
        .setBunRuntime(createRuntime()).build();
  }

  private static File codeSourceOf(Class<?> type) throws URISyntaxException {
    return new File(type.getProtectionDomain().getCodeSource().getLocation().toURI());
  }

  private record Project(Path root, Path frontend, Path sources, Path classes) {

    static Project create(Path dir) throws IOException, URISyntaxException {
      Path frontend = dir.resolve("frontend");
      Path sources = dir.resolve("sources");
      copyTree(fixtureRoot.resolve("frontend"), frontend);
      copyTree(fixtureRoot.resolve("sources"), sources);
      Path classes = Files.createDirectories(dir.resolve("target/classes"));

      return new Project(dir, frontend, sources, classes);
    }

    BundlerExecution.Request request() throws URISyntaxException {
      return new BundlerExecution.Request().setProjectName("bundler-it")
          .setClasspathRoots(List.of(codeSourceOf(CardView.class), codeSourceOf(BundleEntry.class)))
          .setBundleSourceRoot(frontend).setWorkDir(root.resolve("target/bundle"))
          .setClassesOutputDir(classes).setNpmRoot(root).setSourceScanRoots(List.of(sources))
          .setExtensionOverrides(Map.of(TAILWIND_ID, true));
    }

    private static void copyTree(Path from, Path to) throws IOException {
      try (Stream<Path> files = Files.walk(from)) {
        for (Path file : files.toList()) {
          Path target = to.resolve(from.relativize(file).toString());
          if (Files.isDirectory(file)) {
            Files.createDirectories(target);
          } else {
            Files.copy(file, target, StandardCopyOption.REPLACE_EXISTING);
          }
        }
      }
    }
  }

  private static final class RecordingLogger implements BundleLogger {

    private final List<String> lines = new CopyOnWriteArrayList<>();
    private final List<String> warnings = new CopyOnWriteArrayList<>();

    @Override
    public void log(System.Logger.Level level, String message) {
      System.out.println("[bundler " + level + "] " + message);
      lines.add(message);
      if (level.getSeverity() >= System.Logger.Level.WARNING.getSeverity()) {
        warnings.add(message);
      }
    }

    boolean awaitLines(String fragment, int count, Duration timeout) throws InterruptedException {
      long deadline = System.nanoTime() + timeout.toNanos();
      while (System.nanoTime() < deadline) {
        if (lines.stream().filter(line -> line.contains(fragment)).count() >= count) {
          return true;
        }

        Thread.sleep(50);
      }

      return false;
    }
  }
}
