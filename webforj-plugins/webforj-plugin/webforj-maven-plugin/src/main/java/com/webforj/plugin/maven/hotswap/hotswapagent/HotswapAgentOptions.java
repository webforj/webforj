package com.webforj.plugin.maven.hotswap.hotswapagent;

import java.io.File;

/**
 * Configures the HotswapAgent attachment.
 *
 * <p>
 * An empty element is a complete configuration. The plugin then downloads the default agent version
 * and caches it under {@code ~/.webforj/hotswap-agent}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HotswapAgentOptions {

  private String version;
  private File path;

  /**
   * The agent version to download when no path is supplied.
   *
   * @return the pinned version, or null for the default
   */
  public String getVersion() {
    return version;
  }

  /**
   * Sets the agent version to download.
   *
   * @param version the version
   * @return this options instance
   */
  public HotswapAgentOptions setVersion(String version) {
    this.version = version;
    return this;
  }

  /**
   * Optional path to an agent jar already on disk.
   *
   * <p>
   * When set, the plugin uses this jar directly instead of downloading.
   * </p>
   *
   * @return the agent jar, or null to download
   */
  public File getPath() {
    return path;
  }

  /**
   * Sets the agent jar to use directly.
   *
   * @param path the agent jar
   * @return this options instance
   */
  public HotswapAgentOptions setPath(File path) {
    this.path = path;
    return this;
  }
}
