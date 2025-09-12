package com.webforj.spring.security;

import com.webforj.spring.SpringConfigurationProperties;
import com.webforj.spring.security.evaluator.SpringRouteAccessEvaluator;
import java.lang.System.Logger;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextHolderStrategy;

/**
 * Auto-configuration for webforJ Spring Security integration.
 *
 * <p>
 * This configuration is only activated when Spring Security is on the classpath.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@Configuration
@ConditionalOnClass(SecurityContextHolder.class)
@AutoConfigureAfter(SecurityAutoConfiguration.class)
@EnableConfigurationProperties(SpringSecurityConfigurationProperties.class)
public class SpringSecurityAutoConfiguration {
  private static Logger logger = System.getLogger(SpringSecurityAutoConfiguration.class.getName());

  @Bean
  SpringRouteSecurityConfiguration springSecurityConfiguration() {
    logger.log(Logger.Level.DEBUG, "Registering SpringRouteSecurityConfiguration");
    return new SpringRouteSecurityConfiguration();
  }

  @Bean
  SpringRouteSecurityManager springSecurityManager() {
    logger.log(Logger.Level.DEBUG, "Registering SpringRouteSecurityManager");
    return new SpringRouteSecurityManager();
  }

  @Bean
  @ConditionalOnMissingBean
  SpringRouteAccessEvaluator springRouteAccessEvaluator() {
    logger.log(Logger.Level.DEBUG, "Registering SpringRouteAccessEvaluator");
    return new SpringRouteAccessEvaluator();
  }

  @Bean
  @ConditionalOnMissingBean
  WebforjFrameworkRequestMatcher webforjFrameworkRequestMatcher(
      SpringConfigurationProperties properties) {
    logger.log(Logger.Level.DEBUG, "Registering WebforjFrameworkRequestMatcher");
    return new WebforjFrameworkRequestMatcher(properties);
  }

  @Bean
  @ConditionalOnMissingBean(SecurityContextHolderStrategy.class)
  WebforjSecurityContextHolderStrategy webforjSecurityContextHolderStrategy() {
    logger.log(Logger.Level.DEBUG, "Registering WebforjSecurityContextHolderStrategy");
    WebforjSecurityContextHolderStrategy strategy = new WebforjSecurityContextHolderStrategy();
    // Set the strategy so SecurityContextHolder uses it
    SecurityContextHolder.setContextHolderStrategy(strategy);
    return strategy;
  }
}
