package com.webforj.bundle.bun.runtime;

import com.webforj.bundle.bun.BundleLogger;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.commons.lang3.SystemUtils;

/**
 * Resolves a Bun executable for the current platform.
 *
 * <p>
 * Either the caller already has a binary on disk (override path) or this runtime downloads the
 * requested version from the official Bun release archive and caches it under
 * {@code ~/.webforj/bun/<version>}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BunRuntime {

  static final String DEFAULT_RELEASE_HOST = "https://github.com/oven-sh/bun/releases/download";

  /**
   * The Bun version used when a runtime is built without an explicit version. This is the single
   * source of the default across the bundler and the build plugins.
   */
  public static final String DEFAULT_VERSION = "1.3.14";

  // The reported os.arch differs across JVMs and platforms. These are the spellings
  // that mean a 64 bit Intel or 64 bit ARM CPU, the only two CPU families Bun ships.
  private static final Set<String> INTEL_64 = Set.of("x64", "x8664", "amd64", "em64t", "ia32e");
  private static final Set<String> ARM_64 = Set.of("aarch64", "arm64");

  private volatile BundleLogger log = BundleLogger.system();

  private final Path cacheRoot;
  private final String version;
  private final Path overridePath;
  private final String releaseHost;

  BunRuntime(Path cacheRoot, String version, Path overridePath, String releaseHost) {
    this.cacheRoot = cacheRoot;
    this.version = version == null || version.isBlank() ? DEFAULT_VERSION : version;
    this.overridePath = overridePath;
    this.releaseHost = releaseHost;
  }

  /**
   * Creates a new builder for a Bun runtime.
   *
   * @return a new builder
   */
  public static BunRuntimeBuilder create() {
    return new BunRuntimeBuilder();
  }

  /**
   * Sets where this runtime reports its output, so it shares the channel the bundler was given.
   *
   * @param logger the logger, the invoker owns it
   */
  public void setLogger(BundleLogger logger) {
    this.log = logger != null ? logger : BundleLogger.system();
  }

  /**
   * Resolves the Bun binary, downloading and extracting it if needed.
   *
   * @return the path to a runnable Bun binary
   * @throws IOException if download or extraction fails
   * @throws InterruptedException if the download is interrupted
   */
  public Path resolve() throws IOException, InterruptedException {
    if (overridePath != null) {
      if (!Files.isExecutable(overridePath)) {
        throw new IOException("bun override path is not executable: " + overridePath);
      }

      return overridePath;
    }

    Path versionDir = cacheRoot.resolve(version);
    Path binary = versionDir.resolve(getBinaryName());
    if (Files.isExecutable(binary)) {
      return binary;
    }

    Files.createDirectories(versionDir);
    Path archive = versionDir.resolve(getArchiveFileName());
    download(getArchiveUrl(), archive);
    extract(archive, versionDir);

    Files.deleteIfExists(archive);
    if (!Files.isExecutable(binary)) {
      makeExecutable(binary);
    }

    return binary;
  }

  /**
   * Runs Bun with the given arguments and wait for completion.
   *
   * @param workDir the working directory for the process
   * @param args the arguments passed to Bun (without the binary itself)
   * @return the process exit code
   * @throws IOException if the process cannot be started
   * @throws InterruptedException if the wait is interrupted
   */
  public int execute(Path workDir, List<String> args) throws IOException, InterruptedException {
    return execute(workDir, args, null);
  }

  /**
   * Runs Bun with the given arguments and wait for completion, reporting every output line.
   *
   * <p>
   * The line hook receives each line as Bun prints it, so a caller can surface the output where the
   * default debug log would hide it, such as the result of a test run.
   * </p>
   *
   * @param workDir the working directory for the process
   * @param args the arguments passed to Bun (without the binary itself)
   * @param lineHook notified with every output line, may be null
   *
   * @return the process exit code
   * @throws IOException if the process cannot be started
   * @throws InterruptedException if the wait is interrupted
   */
  public int execute(Path workDir, List<String> args, Consumer<String> lineHook)
      throws IOException, InterruptedException {
    Process process = launch(workDir, args);
    try (InputStream out = process.getInputStream()) {
      pump(out, lineHook);
    }

    return process.waitFor();
  }

  /**
   * Starts Bun with the given arguments without waiting for it to finish.
   *
   * <p>
   * Output is streamed to the configured logger on a daemon thread. The returned process belongs to
   * the caller, which is responsible for terminating it.
   * </p>
   *
   * @param workDir the working directory for the process
   * @param args the arguments passed to Bun (without the binary itself)
   *
   * @return the started process
   * @throws IOException if the process cannot be started
   * @throws InterruptedException if resolving the binary is interrupted
   */
  public Process start(Path workDir, List<String> args) throws IOException, InterruptedException {
    return start(workDir, args, null);
  }

  /**
   * Starts Bun in the background and invoke a hook for each output line, in addition to logging it.
   *
   * <p>
   * Lets the caller react to watch rebuild markers.
   * </p>
   *
   * @param workDir the working directory for the process
   * @param args the arguments passed to Bun (without the binary itself)
   * @param lineHook a consumer invoked for every output line, may be null
   * @return the started process
   * @throws IOException if the process cannot be started
   * @throws InterruptedException if resolving the binary is interrupted
   */
  public Process start(Path workDir, List<String> args, Consumer<String> lineHook)
      throws IOException, InterruptedException {
    return start(workDir, args, Map.of(), lineHook);
  }

  /**
   * Starts Bun in the background with extra environment variables, invoking a hook for each output
   * line in addition to logging it.
   *
   * @param workDir the working directory for the process
   * @param args the arguments passed to Bun (without the binary itself)
   * @param env extra environment variables to set for the process
   * @param lineHook a consumer invoked for every output line, may be null
   * @return the started process
   * @throws IOException if the process cannot be started
   * @throws InterruptedException if resolving the binary is interrupted
   */
  public Process start(Path workDir, List<String> args, Map<String, String> env,
      Consumer<String> lineHook) throws IOException, InterruptedException {
    Process process = launch(workDir, args, env);
    Thread pumpThread = new Thread(() -> {
      try (InputStream out = process.getInputStream()) {
        pump(out, lineHook);
      } catch (IOException e) {
        log.debug("bun output stream closed: {}", e.getMessage());
      }
    }, "webforj-bun-watch");
    pumpThread.setDaemon(true);
    pumpThread.start();

    return process;
  }

  private Process launch(Path workDir, List<String> args) throws IOException, InterruptedException {
    return launch(workDir, args, Map.of());
  }

  private Process launch(Path workDir, List<String> args, Map<String, String> env)
      throws IOException, InterruptedException {
    Path binary = resolve();
    ProcessBuilder pb = new ProcessBuilder();
    pb.command().add(binary.toAbsolutePath().toString());
    pb.command().addAll(args);
    pb.directory(workDir.toFile());
    pb.redirectErrorStream(true);
    pb.environment().putAll(env);

    return pb.start();
  }

  private void pump(InputStream out, Consumer<String> lineHook) throws IOException {
    out.transferTo(new OutputStream() {
      private final StringBuilder lineBuf = new StringBuilder();

      @Override
      public void write(int b) {
        if (b == '\n') {
          String line = lineBuf.toString();
          if (lineHook != null) {
            // The hook owns the line and reports it where the caller wants it seen, so the runtime
            // does not also log it and forward it twice.
            lineHook.accept(line);
          } else {
            log.log(System.Logger.Level.DEBUG, line);
          }
          lineBuf.setLength(0);
        } else if (b != '\r') {
          lineBuf.append((char) b);
        }
      }
    });
  }

  String getArchiveUrl() {
    return releaseHost + "/bun-v" + version + "/" + getArchiveFileName();
  }

  String getArchiveFileName() {
    return "bun" + getPlatformSuffix() + ".zip";
  }

  String getBinaryName() {
    return SystemUtils.IS_OS_WINDOWS ? "bun.exe" : "bun";
  }

  private String getPlatformSuffix() {
    return "-" + getOsToken() + "-" + getArchToken();
  }

  private String getOsToken() {
    if (SystemUtils.IS_OS_WINDOWS) {
      return "windows";
    }

    if (SystemUtils.IS_OS_MAC) {
      return "darwin";
    }

    if (SystemUtils.IS_OS_LINUX) {
      return "linux";
    }

    throw new IllegalStateException("Bun publishes no build for this operating system ("
        + System.getProperty("os.name") + "). Set webforj.bundler.path to a local binary.");
  }

  private String getArchToken() {
    String reported = System.getProperty("os.arch", "").toLowerCase(Locale.ROOT);
    String compact = reported.replaceAll("[^a-z0-9]", "");
    if (INTEL_64.contains(compact)) {
      return "x64";
    }

    if (ARM_64.contains(compact)) {
      return "aarch64";
    }

    throw new IllegalStateException("Bun publishes no build for the CPU architecture '" + reported
        + "'. Set webforj.bundler.path to a local binary.");
  }

  private void download(String url, Path target) throws IOException, InterruptedException {
    log.info("downloading bun from {}", url);
    HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS)
        .connectTimeout(Duration.ofSeconds(30)).build();
    HttpRequest request =
        HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(5)).GET().build();
    HttpResponse<InputStream> response =
        client.send(request, HttpResponse.BodyHandlers.ofInputStream());

    if (response.statusCode() != 200) {
      throw new IOException(
          "bun download failed with HTTP " + response.statusCode() + " for " + url);
    }

    try (InputStream body = response.body()) {
      Files.copy(body, target, StandardCopyOption.REPLACE_EXISTING);
    }
  }

  private void extract(Path archive, Path destination) throws IOException {
    Set<String> seen = new HashSet<>();
    try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(archive))) {
      ZipEntry entry;
      while ((entry = zip.getNextEntry()) != null) { // NOSONAR
        if (entry.isDirectory()) {
          continue;
        }
        Path name = Path.of(entry.getName()).getFileName();
        if (name == null) {
          continue;
        }
        if (!seen.add(name.toString())) {
          continue;
        }
        Path out = destination.resolve(name.toString());
        Files.copy(zip, out, StandardCopyOption.REPLACE_EXISTING);
      }
    }
  }

  private void makeExecutable(Path binary) throws IOException {
    if (SystemUtils.IS_OS_WINDOWS) {
      return;
    }

    if (!binary.toFile().setExecutable(true, false)) {
      throw new IOException("could not mark the bun binary executable: " + binary);
    }
  }
}
