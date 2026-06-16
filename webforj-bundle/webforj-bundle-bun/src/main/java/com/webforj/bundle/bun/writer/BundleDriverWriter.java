package com.webforj.bundle.bun.writer;

import com.google.gson.ExclusionStrategy;
import com.google.gson.FieldAttributes;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.webforj.bundle.bun.discovery.BundleEntryDeclaration;
import com.webforj.bundle.bun.plugin.BunPlugin;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Writes the build driver that runs {@code Bun.build}.
 *
 * <p>
 * The driver is a fixed script shipped as a resource, copied next to the generated package.json so
 * its plugin imports resolve against the installed node_modules. Everything that varies per build,
 * the entries, output directory, naming, minify flag, and the plugins to load, is written as JSON
 * the driver reads, so the script itself never changes between builds.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleDriverWriter {

  /** The classpath resource holding the build driver. */
  static final String DRIVER_RESOURCE = "com/webforj/bundle/bun/driver.mjs";

  /** The file name the driver is written as. */
  public static final String DRIVER_NAME = "driver.mjs";

  /** The file name the driver configuration is written as. */
  public static final String CONFIG_NAME = "driver.config.json";

  /** The folder, next to the driver, the curated plugin wrappers are written into. */
  public static final String PLUGINS_DIR = "plugins";

  private static final Gson GSON = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping()
      .serializeNulls().addSerializationExclusionStrategy(new DriverConfigFields()).create();

  /**
   * Copies the build driver into the working directory.
   *
   * @param workDir the directory to write into
   * @return the path of the written driver
   * @throws IOException if the resource cannot be read or written
   */
  public Path writeDriver(Path workDir) throws IOException {
    Files.createDirectories(workDir);
    Path target = workDir.resolve(DRIVER_NAME);
    copyResource(DRIVER_RESOURCE, target);

    return target;
  }

  /**
   * Writes the wrapper module of each plugin next to the driver, under its plugin folder.
   *
   * <p>
   * The driver loads them by the plugin wrapper path, so no import path is written here.
   * </p>
   *
   * @param workDir the directory to write into
   * @param plugins the enabled plugins
   * @throws IOException if a wrapper cannot be written
   */
  public void writePlugins(Path workDir, List<BunPlugin> plugins) throws IOException {
    if (plugins.isEmpty()) {
      return;
    }

    Path pluginDir = workDir.resolve(PLUGINS_DIR);
    for (BunPlugin plugin : plugins) {
      Path target = pluginDir.resolve(plugin.getWrapper());
      Files.createDirectories(target.getParent());
      Files.write(target, plugin.getWrapperContent());
    }
  }

  /**
   * Writes the driver configuration next to the driver.
   *
   * @param workDir the directory to write into
   * @param config the driver configuration
   * @return the path of the written configuration
   * @throws IOException if the file cannot be written
   */
  public Path writeConfig(Path workDir, Config config) throws IOException {
    Files.createDirectories(workDir);
    Path target = workDir.resolve(CONFIG_NAME);
    Files.writeString(target, GSON.toJson(config), StandardCharsets.UTF_8);

    return target;
  }

  private void copyResource(String resource, Path target) throws IOException {
    try (InputStream stream = getClass().getClassLoader().getResourceAsStream(resource)) {
      if (stream == null) {
        throw new IOException("missing bundler resource: " + resource);
      }

      Files.write(target, stream.readAllBytes());
    }
  }

  /**
   * The configuration the build driver consumes.
   */
  public static class Config {

    private List<BundleEntryDeclaration> entries = null;
    private String outdir = null;
    private String root = null;
    private String metafile = null;
    private String entryNaming = null;
    private boolean minify = false;
    private boolean hashed = false;
    private boolean splitting = true;
    private List<BunPlugin> plugins = null;
    private String userConfig = null;
    private List<String> watchPaths = List.of();

    /**
     * Sets the entries to build.
     *
     * @param entries the entries to build
     * @return the config
     */
    public Config setEntries(List<BundleEntryDeclaration> entries) {
      this.entries = entries;

      return this;
    }

    /**
     * Gets the entries to build.
     *
     * @return the entries to build
     * @see #setEntries(List)
     */
    public List<BundleEntryDeclaration> getEntries() {
      return entries;
    }

    /**
     * Sets the absolute output directory.
     *
     * @param outdir the absolute output directory
     * @return the config
     */
    public Config setOutdir(String outdir) {
      this.outdir = outdir;

      return this;
    }

    /**
     * Gets the absolute output directory.
     *
     * @return the absolute output directory
     * @see #setOutdir(String)
     */
    public String getOutdir() {
      return outdir;
    }

    /**
     * Sets the absolute bundle source root.
     *
     * @param root the absolute bundle source root
     * @return the config
     */
    public Config setRoot(String root) {
      this.root = root;

      return this;
    }

    /**
     * Gets the absolute bundle source root.
     *
     * @return the absolute bundle source root
     * @see #setRoot(String)
     */
    public String getRoot() {
      return root;
    }

    /**
     * Sets the absolute path where the driver writes the output to entry mapping.
     *
     * @param metafile the absolute metafile path
     * @return the config
     */
    public Config setMetafile(String metafile) {
      this.metafile = metafile;

      return this;
    }

    /**
     * Gets the absolute path where the driver writes the output to entry mapping.
     *
     * @return the absolute metafile path
     * @see #setMetafile(String)
     */
    public String getMetafile() {
      return metafile;
    }

    /**
     * Sets the bun entry naming pattern.
     *
     * @param entryNaming the bun entry naming pattern
     * @return the config
     */
    public Config setEntryNaming(String entryNaming) {
      this.entryNaming = entryNaming;

      return this;
    }

    /**
     * Gets the bun entry naming pattern.
     *
     * @return the bun entry naming pattern
     * @see #setEntryNaming(String)
     */
    public String getEntryNaming() {
      return entryNaming;
    }

    /**
     * Sets whether to minify.
     *
     * @param minify {@code true} to minify
     * @return the config
     */
    public Config setMinify(boolean minify) {
      this.minify = minify;

      return this;
    }

    /**
     * Indicates whether to minify.
     *
     * @return {@code true} to minify
     * @see #setMinify(boolean)
     */
    public boolean isMinify() {
      return minify;
    }

    /**
     * Sets whether output names carry a content hash.
     *
     * @param hashed {@code true} when output names carry a content hash
     * @return the config
     */
    public Config setHashed(boolean hashed) {
      this.hashed = hashed;

      return this;
    }

    /**
     * Indicates whether output names carry a content hash.
     *
     * @return {@code true} when output names carry a content hash
     * @see #setHashed(boolean)
     */
    public boolean isHashed() {
      return hashed;
    }

    /**
     * Sets whether Bun splits shared code into chunks. Off for an eager bundle so the single output
     * is self contained and can be inlined.
     *
     * @param splitting {@code true} to split shared code into chunks
     * @return the config
     */
    public Config setSplitting(boolean splitting) {
      this.splitting = splitting;

      return this;
    }

    /**
     * Indicates whether Bun splits shared code into chunks.
     *
     * @return {@code true} to split shared code into chunks
     * @see #setSplitting(boolean)
     */
    public boolean isSplitting() {
      return splitting;
    }

    /**
     * Sets the plugins to load.
     *
     * @param plugins the plugins to load
     * @return the config
     */
    public Config setPlugins(List<BunPlugin> plugins) {
      this.plugins = plugins;

      return this;
    }

    /**
     * Gets the plugins to load.
     *
     * @return the plugins to load
     * @see #setPlugins(List)
     */
    public List<BunPlugin> getPlugins() {
      return plugins;
    }

    /**
     * Sets the absolute path to the app bun.config.ts, or null when absent.
     *
     * @param userConfig the absolute path to the app bun.config.ts
     * @return the config
     */
    public Config setUserConfig(String userConfig) {
      this.userConfig = userConfig;

      return this;
    }

    /**
     * Sets the extra directories the watch rebuilds on, beyond the bundle source root.
     *
     * @param watchPaths the absolute directories to watch
     * @return the config
     */
    public Config setWatchPaths(List<String> watchPaths) {
      this.watchPaths = watchPaths;

      return this;
    }

    /**
     * Gets the extra directories the watch rebuilds on, beyond the bundle source root.
     *
     * @return the absolute directories to watch
     * @see #setWatchPaths(List)
     */
    public List<String> getWatchPaths() {
      return watchPaths;
    }

    /**
     * Gets the absolute path to the app bun.config.ts, or null when absent.
     *
     * @return the absolute path to the app bun.config.ts
     * @see #setUserConfig(String)
     */
    public String getUserConfig() {
      return userConfig;
    }
  }

  /**
   * Keeps the driver configuration JSON to the fields the driver reads, the entry source and build
   * path and the plugin id and wrapper, leaving the Java only fields of the shared models out.
   */
  private static final class DriverConfigFields implements ExclusionStrategy {

    @Override
    public boolean shouldSkipField(FieldAttributes field) {
      Class<?> owner = field.getDeclaringClass();
      String name = field.getName();
      if (owner == BundleEntryDeclaration.class) {
        return "file".equals(name) || "npm".equals(name) || "owners".equals(name);
      }

      if (owner == BunPlugin.class) {
        return "wrapperContent".equals(name);
      }

      return false;
    }

    @Override
    public boolean shouldSkipClass(Class<?> type) {
      return false;
    }
  }
}
