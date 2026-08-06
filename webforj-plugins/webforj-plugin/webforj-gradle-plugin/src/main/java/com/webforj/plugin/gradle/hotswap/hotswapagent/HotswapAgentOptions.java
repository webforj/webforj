package com.webforj.plugin.gradle.hotswap.hotswapagent;

import org.gradle.api.file.RegularFileProperty;
import org.gradle.api.provider.Property;

/**
 * Configures the HotswapAgent attachment.
 *
 * <p>
 * An empty block is a complete configuration. The plugin then downloads the default agent version
 * and caches it under {@code ~/.webforj/hotswap-agent}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class HotswapAgentOptions {

  /**
   * The agent version to download when no path is supplied.
   *
   * @return the pinned version property
   */
  public abstract Property<String> getVersion();

  /**
   * Optional path to an agent jar already on disk.
   *
   * <p>
   * When set, the plugin uses this jar directly instead of downloading.
   * </p>
   *
   * @return the agent jar property
   */
  public abstract RegularFileProperty getPath();
}
