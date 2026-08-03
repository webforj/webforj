package com.webforj.plugin.gradle.hotswap;

import com.webforj.plugin.gradle.hotswap.jrebel.JrebelConfiguration;
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
public abstract class HotswapConfiguration {

  private boolean jrebelConfigured;

  /**
   * The JRebel configuration values.
   *
   * @return the JRebel configuration
   */
  @Nested
  public abstract JrebelConfiguration getJrebel();

  /**
   * Attaches JRebel and configures it.
   *
   * @param action the configuration action
   */
  public void jrebel(Action<? super JrebelConfiguration> action) {
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
