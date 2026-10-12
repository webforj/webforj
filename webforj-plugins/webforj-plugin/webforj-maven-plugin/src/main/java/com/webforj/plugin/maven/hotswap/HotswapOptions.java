package com.webforj.plugin.maven.hotswap;

import com.webforj.plugin.maven.hotswap.hotswapagent.HotswapAgentOptions;
import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import java.io.File;

/**
 * Configures the hotswap integration of the watch goal.
 *
 * <p>
 * At most one nested tool is configured, because two agents cannot share the application virtual
 * machine.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HotswapOptions {

  private HotswapAgentOptions hotswapAgent;
  private JrebelOptions jrebel;
  private File directory;

  /**
   * The HotswapAgent configuration, present when the build attaches HotswapAgent.
   *
   * @return the HotswapAgent configuration, or null when not configured
   */
  public HotswapAgentOptions getHotswapAgent() {
    return hotswapAgent;
  }

  /**
   * Sets the HotswapAgent configuration.
   *
   * @param hotswapAgent the HotswapAgent configuration
   * @return this options instance
   */
  public HotswapOptions setHotswapAgent(HotswapAgentOptions hotswapAgent) {
    this.hotswapAgent = hotswapAgent;
    return this;
  }

  /**
   * The JRebel configuration, present when the build attaches JRebel.
   *
   * @return the JRebel configuration, or null when not configured
   */
  public JrebelOptions getJrebel() {
    return jrebel;
  }

  /**
   * Sets the JRebel configuration.
   *
   * @param jrebel the JRebel configuration
   * @return this options instance
   */
  public HotswapOptions setJrebel(JrebelOptions jrebel) {
    this.jrebel = jrebel;
    return this;
  }

  /**
   * The hotswap directory holding the downloaded agent and the options file of a shared container.
   *
   * @return the hotswap directory, or null for the default under the user home
   */
  public File getDirectory() {
    return directory;
  }

  /**
   * Sets the hotswap directory.
   *
   * @param directory the hotswap directory
   * @return this options instance
   */
  public HotswapOptions setDirectory(File directory) {
    this.directory = directory;
    return this;
  }
}
