package com.webforj.spring.scope;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class that registers webforJ custom scopes with Spring.
 *
 * <p>
 * This configuration registers the following custom scope:
 * <ul>
 * <li>{@code webforj-environment} - Scoped to the webforJ Environment (request) lifecycle</li>
 * </ul>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@Configuration
public class WebforjScopeConfiguration implements BeanFactoryPostProcessor {
  private static final Logger logger = System.getLogger(WebforjScopeConfiguration.class.getName());

  @Override
  public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) {
    // Register Environment scope
    EnvironmentScopeProcessor environmentScope = new EnvironmentScopeProcessor();
    beanFactory.registerScope("webforj-environment", environmentScope);
    logger.log(Level.DEBUG, "Registered webforj-environment scope");
  }
}
