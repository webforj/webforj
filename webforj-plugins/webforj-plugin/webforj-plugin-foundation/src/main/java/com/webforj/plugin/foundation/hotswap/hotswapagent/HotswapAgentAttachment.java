package com.webforj.plugin.foundation.hotswap.hotswapagent;

import com.sun.management.HotSpotDiagnosticMXBean;
import com.webforj.plugin.foundation.hotswap.HotswapAttachment;
import java.io.IOException;
import java.io.InputStream;
import java.lang.management.ManagementFactory;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.function.Predicate;

/**
 * Attaches HotswapAgent to the application virtual machine.
 *
 * <p>
 * Either the caller already has the agent jar on disk (override path) or this attachment downloads
 * the requested version from Maven Central, verifies it against the published checksum, and caches
 * it under the given cache root. The attachment checks whether the virtual machine supports
 * enhanced class redefinition. A machine that does gets the capability switched on for the run. A
 * machine that does not still gets the agent, limited to method body changes, and the limitation is
 * reported with the requirement named, so the user knows what applies and what does not.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapAgentAttachment implements HotswapAttachment {

  /** The package holding the forwarder the agent discovers, shipped in webforj-devtools. */
  static final String PLUGIN_PACKAGES = "com.webforj.devtools.hotswap";
  static final String PROPERTIES_FILE_NAME = "hotswap-agent.properties";
  static final String REDEFINITION_OPTION = "AllowEnhancedClassRedefinition";
  static final String REDEFINITION_FLAG = "-XX:+" + REDEFINITION_OPTION;
  static final String TOOL_ARGUMENT = "-Dwebforj.hotswap.tool=hotswapAgent";
  static final String LEVEL_ARGUMENT_PREFIX = "-Dwebforj.hotswap.level=";

  static final String DEFAULT_REPOSITORY_HOST = "https://repo1.maven.org/maven2";
  static final String GROUP_ID = "org.hotswapagent";
  static final String ARTIFACT_ID = "hotswap-agent";

  /**
   * The HotswapAgent version used when the build pins none. This is the single source of the
   * default across the build plugins.
   */
  public static final String DEFAULT_VERSION = "2.0.3";

  private static final long CHECK_TIMEOUT_SECONDS = 30;

  private final Path cacheRoot;
  private final String version;
  private final Path overridePath;
  private final Path configurationDirectory;
  private final String repositoryHost;
  private final Path javaExecutable;
  private final Predicate<String> runningVirtualMachineOptions;
  private final Consumer<String> log;
  private final Consumer<String> warn;

  private HotswapAgentAttachment(Builder builder) {
    this.cacheRoot = builder.cacheRoot;
    this.version =
        builder.version == null || builder.version.isBlank() ? DEFAULT_VERSION : builder.version;
    this.overridePath = builder.overridePath;
    this.configurationDirectory = builder.configurationDirectory;
    this.repositoryHost = builder.repositoryHost;
    this.javaExecutable = builder.javaExecutable;
    this.runningVirtualMachineOptions = builder.runningVirtualMachineOptions;
    this.log = builder.log;
    this.warn = builder.warn;
  }

  /**
   * Creates a new builder for an attachment.
   *
   * @return a new builder
   */
  public static Builder create() {
    return new Builder();
  }

  /**
   * The version this attachment resolves.
   *
   * @return the resolved version
   */
  public String getVersion() {
    return version;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public List<String> arguments() throws IOException {
    boolean enhanced = supportsEnhancedRedefinition();

    Path jar = resolve();
    Path propertiesFile = writeProperties();
    log.accept("webforJ hotswap: HotswapAgent " + version
        + " attached to the application virtual machine");

    // autoHotswap must travel as an agent argument. The agent reads it only from this line, a
    // value in the properties file never turns the automatic mode on.
    List<String> arguments = new ArrayList<>();
    if (enhanced) {
      // The flag only exists on a machine that supports the capability, anywhere else it ends the
      // start of the virtual machine, so it travels only after the check proved it.
      arguments.add(REDEFINITION_FLAG);
    }

    // The agent log level keeps the reload lines and the warnings while its routine chatter
    // stays out of the application console.
    arguments.add("-javaagent:" + jar.toAbsolutePath()
        + "=autoHotswap=true,LOGGER=warning,propertiesFilePath=" + propertiesFile.toAbsolutePath());

    // Each open stays one self contained token
    arguments.addAll(List.of("--add-opens=java.base/java.lang=ALL-UNNAMED",
        "--add-opens=java.base/java.io=ALL-UNNAMED",
        "--add-opens=java.desktop/java.beans=ALL-UNNAMED"));

    // The properties tell the application which tool this attachment installed and how deep its
    // updates go on this machine, so the running application can tell the developer.
    arguments.add(TOOL_ARGUMENT);
    arguments.add(LEVEL_ARGUMENT_PREFIX + (enhanced ? "full" : "limited"));

    return List.copyOf(arguments);
  }

  String getJarUrl() {
    return repositoryHost + "/" + GROUP_ID.replace('.', '/') + "/" + ARTIFACT_ID + "/" + version
        + "/" + getJarFileName();
  }

  String getJarFileName() {
    return ARTIFACT_ID + "-" + version + ".jar";
  }

  private boolean supportsEnhancedRedefinition() {
    if (javaExecutable == null) {
      // No build system named a fork executable, so the application runs in the same virtual
      // machine as this code and the question is answered in process.
      if (runningVirtualMachineOptions.test(REDEFINITION_OPTION)) {
        return true;
      }

      warnLimitedCapability(
          "the current virtual machine does not support enhanced class redefinition");
      return false;
    }

    if (!Files.isRegularFile(javaExecutable)) {
      warnLimitedCapability("the java executable was not found at " + javaExecutable);
      return false;
    }

    Process check;
    try {
      // The flag is passed to a bare version print. A virtual machine that knows the flag prints
      // the version and exits cleanly, one that does not rejects the unknown flag and exits with
      // an error
      check = new ProcessBuilder(javaExecutable.toString(), REDEFINITION_FLAG, "-version")
          .redirectErrorStream(true).start();
    } catch (IOException e) {
      warnLimitedCapability("the capability check could not start: " + e.getMessage());
      return false;
    }

    try {
      if (!check.waitFor(CHECK_TIMEOUT_SECONDS, TimeUnit.SECONDS)) {
        check.destroyForcibly();
        warnLimitedCapability(
            "the capability check did not answer within " + CHECK_TIMEOUT_SECONDS + " seconds");
        return false;
      }
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      check.destroyForcibly();
      warnLimitedCapability("the capability check was interrupted");
      return false;
    }

    if (check.exitValue() != 0) {
      warnLimitedCapability(
          "the current virtual machine does not support enhanced class redefinition");
      return false;
    }

    return true;
  }

  private void warnLimitedCapability(String reason) {
    // The block mirrors the banner JRebel prints at start, one framed paragraph the eye cannot
    // scroll past, because a single line drowns between the other build output.
    String frame = "#".repeat(76);
    List<String> lines = List.of(frame, "", reason + ".", "",
        "Only method body changes will apply in place. Class structure changes,",
        "a new field or a new method for example, will not reach the running",
        "application at all until it is restarted.", "",
        "Full depth hotswap needs the JetBrains Runtime or another virtual",
        "machine accepting " + REDEFINITION_FLAG, "", frame);
    for (String line : lines) {
      warn.accept(("webforJ hotswap: " + line).stripTrailing());
    }
  }

  private Path resolve() throws IOException {
    if (overridePath != null) {
      if (!Files.isRegularFile(overridePath)) {
        throw new IOException("the HotswapAgent jar override does not exist: " + overridePath);
      }

      return overridePath;
    }

    Path versionDir = cacheRoot.resolve(version);
    Path jar = versionDir.resolve(getJarFileName());
    if (Files.isRegularFile(jar)) {
      return jar;
    }

    Files.createDirectories(versionDir);
    String jarUrl = getJarUrl();
    log.accept("webforJ hotswap: downloading HotswapAgent " + version + " from " + jarUrl);

    // Every download stages to its own file, so two builds racing on a cold cache can never write
    // into one another. The rename is the commit point, so a jar found in the cache later is
    // always a complete and verified download, never a partial one from an interrupted build.
    Path staged = Files.createTempFile(versionDir, getJarFileName() + ".", ".part");
    try {
      download(jarUrl, staged);
      verify(staged, fetchChecksum(jarUrl + ".sha1"), jarUrl);
      Files.move(staged, jar, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
    } finally {
      Files.deleteIfExists(staged);
    }

    return jar;
  }

  private Path writeProperties() throws IOException {
    Files.createDirectories(configurationDirectory);
    Path file = configurationDirectory.resolve(PROPERTIES_FILE_NAME);
    Files.writeString(file, "pluginPackages=" + PLUGIN_PACKAGES + "\n", StandardCharsets.UTF_8);

    return file;
  }

  private void download(String url, Path target) throws IOException {
    HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS)
        .connectTimeout(Duration.ofSeconds(30)).build();
    HttpRequest request =
        HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(5)).GET().build();
    HttpResponse<InputStream> response =
        send(client, request, HttpResponse.BodyHandlers.ofInputStream());

    if (response.statusCode() != 200) {
      throw new IOException(
          "the HotswapAgent download failed with HTTP " + response.statusCode() + " for " + url);
    }

    try (InputStream body = response.body()) {
      Files.copy(body, target, StandardCopyOption.REPLACE_EXISTING);
    }
  }

  private String fetchChecksum(String url) throws IOException {
    HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS)
        .connectTimeout(Duration.ofSeconds(30)).build();
    HttpRequest request =
        HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofMinutes(1)).GET().build();
    HttpResponse<String> response = send(client, request, HttpResponse.BodyHandlers.ofString());

    if (response.statusCode() != 200) {
      throw new IOException(
          "the HotswapAgent checksum failed with HTTP " + response.statusCode() + " for " + url);
    }

    // The repository publishes the checksum as hexadecimal text, sometimes followed by the file
    // name in the style of the sha1sum tool.
    return response.body().trim().split("\\s+")[0].toLowerCase(Locale.ROOT);
  }

  private static <T> HttpResponse<T> send(HttpClient client, HttpRequest request,
      HttpResponse.BodyHandler<T> handler) throws IOException {
    try {
      return client.send(request, handler);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new IOException("the HotswapAgent download was interrupted", e);
    }
  }

  private void verify(Path jar, String expected, String url) throws IOException {
    MessageDigest digest;
    try {
      digest = MessageDigest.getInstance("SHA-1");
    } catch (NoSuchAlgorithmException e) {
      throw new IOException("the virtual machine offers no SHA-1 digest", e);
    }

    try (InputStream in = Files.newInputStream(jar)) {
      byte[] buffer = new byte[8192];
      int read;
      while ((read = in.read(buffer)) != -1) {
        digest.update(buffer, 0, read);
      }
    }

    String actual = HexFormat.of().formatHex(digest.digest());
    if (!actual.equals(expected)) {
      throw new IOException("the HotswapAgent download from " + url
          + " does not match the published checksum, expected " + expected + " but the file has "
          + actual);
    }
  }

  private static boolean runningVirtualMachineHasOption(String option) {
    HotSpotDiagnosticMXBean diagnostics =
        ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean.class);
    if (diagnostics == null) {
      return false;
    }

    try {
      return diagnostics.getVMOption(option) != null;
    } catch (IllegalArgumentException e) {
      // The virtual machine answers an unknown option with this exception, which is the answer.
      return false;
    }
  }

  /**
   * Builds a {@link HotswapAgentAttachment}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.02
   */
  public static final class Builder {

    private Path cacheRoot;
    private String version;
    private Path overridePath;
    private Path configurationDirectory;
    private String repositoryHost = DEFAULT_REPOSITORY_HOST;
    private Path javaExecutable;
    private Predicate<String> runningVirtualMachineOptions =
        HotswapAgentAttachment::runningVirtualMachineHasOption;
    private Consumer<String> log = line -> {
    };
    private Consumer<String> warn = line -> {
    };

    private Builder() {}

    /**
     * Sets the cache root the downloaded jars live under.
     *
     * @param cacheRoot the cache root
     * @return this builder
     */
    public Builder setCacheRoot(Path cacheRoot) {
      this.cacheRoot = cacheRoot;
      return this;
    }

    /**
     * Sets the version to resolve, or null for the default.
     *
     * @param version the version
     * @return this builder
     */
    public Builder setVersion(String version) {
      this.version = version;
      return this;
    }

    /**
     * Sets a jar already on disk, which skips the download entirely.
     *
     * @param overridePath the jar path, or null to download
     * @return this builder
     */
    public Builder setOverridePath(Path overridePath) {
      this.overridePath = overridePath;
      return this;
    }

    /**
     * Sets the directory the generated agent configuration is written into.
     *
     * @param configurationDirectory the configuration directory
     * @return this builder
     */
    public Builder setConfigurationDirectory(Path configurationDirectory) {
      this.configurationDirectory = configurationDirectory;
      return this;
    }

    /**
     * Sets the repository host the jar downloads from.
     *
     * @param repositoryHost the repository host
     * @return this builder
     */
    Builder setRepositoryHost(String repositoryHost) {
      this.repositoryHost = repositoryHost;
      return this;
    }

    /**
     * Sets the java executable the capability check runs, as named by a build system.
     *
     * <p>
     * With null the check asks the running virtual machine directly, which covers every runner
     * whose application runs in the current one.
     * </p>
     *
     * @param javaExecutable the java executable, or null to ask the running virtual machine
     * @return this builder
     */
    public Builder setJavaExecutable(Path javaExecutable) {
      this.javaExecutable = javaExecutable;
      return this;
    }

    /**
     * Sets how the capability check reads an option of the running virtual machine.
     *
     * @param runningVirtualMachineOptions answers whether the running virtual machine knows the
     *        given option
     * @return this builder
     */
    Builder setRunningVirtualMachineOptions(Predicate<String> runningVirtualMachineOptions) {
      this.runningVirtualMachineOptions = runningVirtualMachineOptions;
      return this;
    }

    /**
     * Sets where progress lines are reported.
     *
     * @param log the log sink
     * @return this builder
     */
    public Builder setLog(Consumer<String> log) {
      this.log = log != null ? log : line -> {
      };
      return this;
    }

    /**
     * Sets where warnings are reported.
     *
     * @param warn the warning sink
     * @return this builder
     */
    public Builder setWarn(Consumer<String> warn) {
      this.warn = warn != null ? warn : line -> {
      };
      return this;
    }

    /**
     * Builds the attachment.
     *
     * @return the attachment
     */
    public HotswapAgentAttachment build() {
      return new HotswapAgentAttachment(this);
    }
  }
}
