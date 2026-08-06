package com.webforj.plugin.gradle.hotswap;

import com.webforj.plugin.gradle.hotswap.hotswapagent.HotswapAgentOptions;
import com.webforj.plugin.gradle.hotswap.jrebel.JrebelOptions;
import org.gradle.api.Action;
import org.gradle.api.tasks.Nested;

/**
 * Configures the hotswap integration for a Gradle build.
 *
 * <p>
 * At most one tool block is configured, because two agents cannot share the application virtual
 * machine.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public abstract class HotswapOptions {

  private boolean hotswapAgentConfigured;
  private boolean jrebelConfigured;

  /**
   * The HotswapAgent configuration values.
   *
   * @return the HotswapAgent configuration
   */
  @Nested
  public abstract HotswapAgentOptions getHotswapAgent();

  /**
   * Attaches HotswapAgent and configures it.
   *
   * @param action the configuration action
   */
  public void hotswapAgent(Action<? super HotswapAgentOptions> action) {
    hotswapAgentConfigured = true;
    action.execute(getHotswapAgent());
  }

  /**
   * Whether the build configured HotswapAgent, through the configuration block or by setting a
   * value on it directly.
   *
   * @return true when HotswapAgent was configured
   */
  public boolean isHotswapAgentConfigured() {
    return hotswapAgentConfigured || getHotswapAgent().getVersion().isPresent()
        || getHotswapAgent().getPath().isPresent();
  }

  /**
   * The JRebel configuration values.
   *
   * @return the JRebel configuration
   */
  @Nested
  public abstract JrebelOptions getJrebel();

  /**
   * Attaches JRebel and configures it.
   *
   * @param action the configuration action
   */
  public void jrebel(Action<? super JrebelOptions> action) {
    jrebelConfigured = true;
    action.execute(getJrebel());
  }

  /**
   * Whether the build configured JRebel, through the configuration block or by setting the agent
   * path on it directly.
   *
   * @return true when JRebel was configured
   */
  public boolean isJrebelConfigured() {
    return jrebelConfigured || getJrebel().getPath().isPresent();
  }
}
