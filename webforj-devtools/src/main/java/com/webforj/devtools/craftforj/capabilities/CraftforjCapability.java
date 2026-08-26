package com.webforj.devtools.craftforj.capabilities;

import com.webforj.App;

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
}
