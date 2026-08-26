package com.webforj.devtools.craftforj.capabilities;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;

/**
 * One feature the panel may use, decided by a check over the running application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public interface CraftforjCapability {

  /**
   * Gets the key the panel receives.
   *
   * @return the key
   */
  String getKey();

  /**
   * Decides whether the capability is announced for the given application. The check may look at
   * anything, the configuration, the application class, the classpath, the framework, or another
   * capability.
   *
   * @param app the running application
   * @return {@code true} when the capability is announced
   */
  boolean isSupported(App app);

  /**
   * Reads a switch from the application configuration. A missing key counts as on.
   *
   * @param configKey the configuration key
   * @return {@code true} when the key is absent or set to true
   */
  default boolean isEnabled(String configKey) {
    Environment environment = Environment.getCurrent();
    Config config = environment == null ? null : environment.getConfig();

    return config == null || !config.hasPath(configKey) || config.getBoolean(configKey);
  }
}
