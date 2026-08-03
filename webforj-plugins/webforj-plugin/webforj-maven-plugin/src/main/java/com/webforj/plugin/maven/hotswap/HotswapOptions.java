package com.webforj.plugin.maven.hotswap;

import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;

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

  private JrebelOptions jrebel;

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
}
