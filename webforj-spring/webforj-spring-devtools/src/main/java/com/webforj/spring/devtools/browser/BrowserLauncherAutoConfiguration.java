package com.webforj.spring.devtools.browser;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

/**
 * Auto-configuration for browser launcher.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@Configuration
@ConditionalOnProperty(prefix = "webforj.devtools.browser", name = "open", havingValue = "true")
public class BrowserLauncherAutoConfiguration {

  @Bean
  BrowserLauncher browserLauncher(Environment environment) {
    return new BrowserLauncher(environment);
  }
}
