package com.webforj.spring.security;

import com.webforj.spring.SpringConfigurationProperties;
import com.webforj.spring.security.evaluator.SpringRouteAccessEvaluator;
import java.lang.System.Logger;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.security.autoconfigure.SecurityAutoConfiguration;
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

  /**
   * Installs the {@link WebforjSecurityContextHolderStrategy} in the {@link SecurityContextHolder}
   * before any bean is created, unless the application defines a
   * {@link SecurityContextHolderStrategy} bean of its own.
   *
   * @return the {@link BeanFactoryPostProcessor} that installs the strategy
   */
  @Bean
  static BeanFactoryPostProcessor webforjSecurityContextHolderStrategyInstaller() {
    return (ConfigurableListableBeanFactory beanFactory) -> {
      if (beanFactory.getBeanNamesForType(SecurityContextHolderStrategy.class, true,
          false).length > 0) {
        logger.log(Logger.Level.DEBUG,
            "Keeping the SecurityContextHolderStrategy bean of the application");
        return;
      }

      // The strategy is installed statically and not published as a bean. The security filter
      // chain and the test support of Spring Security both prefer a SecurityContextHolderStrategy
      // bean, and the filter chain clears that bean after every request, which drops the user a
      // test placed into it before its second request.
      SecurityContextHolder.setContextHolderStrategy(new WebforjSecurityContextHolderStrategy());
      logger.log(Logger.Level.DEBUG, "Installed WebforjSecurityContextHolderStrategy");
    };
  }

  @Bean
  @ConditionalOnMissingBean
  WebforjAuthenticationSuccessHandler webforjAuthenticationSuccessHandler() {
    logger.log(Logger.Level.DEBUG, "Registering WebforjAuthenticationSuccessHandler");
    return new WebforjAuthenticationSuccessHandler();
  }
}
