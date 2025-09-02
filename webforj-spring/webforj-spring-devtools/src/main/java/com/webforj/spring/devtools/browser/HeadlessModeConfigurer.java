package com.webforj.spring.devtools.browser;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.env.ConfigurableEnvironment;

/**
 * Configures the JVM headless mode based on browser opening settings. When browser opening is
 * enabled, this processor disables headless mode to allow Desktop API functionality.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class HeadlessModeConfigurer implements EnvironmentPostProcessor {

  @Override
  public void postProcessEnvironment(ConfigurableEnvironment environment,
      SpringApplication application) {
    // Check if browser opening is enabled
    Boolean browserOpen =
        environment.getProperty("webforj.devtools.browser.open", Boolean.class, false);

    if (browserOpen) {
      // Disable headless mode to enable Desktop API functionality
      System.setProperty("java.awt.headless", "false");
    }
  }
}
