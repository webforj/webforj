package com.webforj.spring;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigValueFactory;
import com.webforj.i18n.BundleTranslationResolver;
import com.webforj.i18n.LocaleUtils;
import com.webforj.i18n.TranslationResolver;
import com.webforj.servlet.WebforjServlet;
import com.webforj.spring.scope.SpringScopeCleanup;
import com.webforj.spring.scope.processor.EnvironmentScopeProcessor;
import com.webforj.spring.scope.processor.RouteScopeProcessor;
import com.webforj.spring.scope.processor.SessionScopeProcessor;
import java.lang.System.Logger;
import java.util.List;
import java.util.Locale;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Auto-configuration for the Webforj servlet.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Configuration
@ConditionalOnClass(ServletContextInitializer.class)
@AutoConfigureBefore(WebMvcAutoConfiguration.class)
@EnableConfigurationProperties(SpringConfigurationProperties.class)
@Import(WebforjServletConfiguration.class)
public class SpringAutoConfiguration {
  private static Logger logger = System.getLogger(SpringAutoConfiguration.class.getName());

  /**
   * Creates the webforJ configuration from Spring properties.
   *
   * @param properties the Spring configuration properties
   * @return the webforJ configuration
   */
  @Bean(name = "webforjConfig")
  Config webforjConfig(SpringConfigurationProperties properties) {
    Config config = WebforjConfigBuilder.buildConfig(properties);
    logger.log(Logger.Level.DEBUG, "Built webforJ configuration from Spring properties");
    return config;
  }

  /**
   * Registers the {@link WebforjServlet}.
   *
   * <p>
   * The servlet is mapped based on the {@code webforj.servlet-mapping} property. By default, this
   * is "/*", but users can change it via their application configuration. When mapped to the root
   * context ({@code /*}), the servlet is registered at an internal path and Spring's
   * {@code DispatcherServlet} handles request routing to enable Spring MVC endpoints to coexist
   * with webforJ routes.
   * </p>
   *
   * @param properties the Spring configuration properties
   * @param webforjConfig the webforJ configuration
   *
   * @return the {@link ServletRegistrationBean} for the {@link WebforjServlet}
   */
  @Bean
  ServletRegistrationBean<WebforjServlet> webforjServletRegistration(
      SpringConfigurationProperties properties, Config webforjConfig) {
    String mapping = properties.getServletMapping();
    boolean rootMapping = ServletMappingCondition.isRootMapping(mapping);
    Config finalConfig = webforjConfig;

    if (rootMapping) {
      // When root mapped, register at internal path for MVC integration
      mapping = WebforjServletConfiguration.WEBFORJ_SERVLET_MAPPING;
      logger.log(Logger.Level.INFO,
          "Root mapping detected, registering WebforjServlet at internal path: " + mapping);

      // configure root path for webforJ router. when root mapped, the router root is "/"
      // instead of the context path
      finalConfig =
          webforjConfig.withValue("webforj.router.root", ConfigValueFactory.fromAnyRef("/"));
    } else {
      logger.log(Logger.Level.DEBUG, "Registering WebforjServlet with direct mapping: " + mapping);
    }

    // Set the configuration for the servlet
    WebforjServlet.setConfig(finalConfig);
    logger.log(Logger.Level.DEBUG, "Set webforj configuration for servlet");

    WebforjServlet webforjServlet = new WebforjServlet();

    ServletRegistrationBean<WebforjServlet> registrationBean =
        new ServletRegistrationBean<>(webforjServlet, mapping);

    // Set servlet name for forwarding controller reference
    registrationBean.setName(WebforjServletConfiguration.WEBFORJ_SERVLET_NAME);
    registrationBean.setLoadOnStartup(1);

    logger.log(Logger.Level.DEBUG, "WebforjServlet registered with name '"
        + WebforjServletConfiguration.WEBFORJ_SERVLET_NAME + "' at mapping: " + mapping);

    return registrationBean;
  }

  /**
   * Creates the component registrar that automatically registers webforj components as Spring
   * beans.
   *
   * <p>
   * This registrar will:
   * </p>
   * <ul>
   * <li>Scan for classes annotated with {@code @Route}</li>
   * <li>Register {@code @Route} classes as PROTOTYPE and LAZY Spring beans</li>
   * <li>Skip classes already managed by Spring (e.g., {@code @Component}, {@code @Service})</li>
   * <li>Only scan packages specified in {@code @Routify.packages()} or the application package</li>
   * </ul>
   *
   * @return the {@link ComponentRegistrar}
   */
  @Bean
  static ComponentRegistrar webforjComponentRegistrar() {
    return new ComponentRegistrar();
  }

  /**
   * Injects the Spring {@link ApplicationContext} into the current {@link ContextHolder}.
   *
   * @return the {@link ContextInjector}
   */
  @Bean
  ContextInjector webforjContextInjector() {
    return new ContextInjector();
  }

  /**
   * Registers webforJ custom scopes with Spring.
   *
   * <p>
   * This post processor registers the following custom scopes:
   * <ul>
   * <li>{@code webforj-environment} - Scoped to the webforJ Environment (request) lifecycle</li>
   * <li>{@code webforj-route} - Scoped to the webforJ route hierarchy lifecycle</li>
   * <li>{@code webforj-session} - Scoped to the webforJ session lifecycle</li>
   * </ul>
   * </p>
   *
   * @return the {@link BeanFactoryPostProcessor} that registers the scopes
   */
  @Bean
  static BeanFactoryPostProcessor webforjScopeRegistrar() {
    return (ConfigurableListableBeanFactory beanFactory) -> {
      // Register Environment scope
      EnvironmentScopeProcessor environmentScope = new EnvironmentScopeProcessor();
      beanFactory.registerScope("webforj-environment", environmentScope);
      logger.log(Logger.Level.DEBUG, "Registered webforj-environment scope");

      // Register Route scope
      RouteScopeProcessor routeScope = new RouteScopeProcessor();
      routeScope.postProcessBeanFactory(beanFactory);
      logger.log(Logger.Level.DEBUG, "Registered webforj-route scope");

      // Register Session scope
      SessionScopeProcessor sessionScope = new SessionScopeProcessor();
      beanFactory.registerScope("webforj-session", sessionScope);
      logger.log(Logger.Level.DEBUG, "Registered webforj-session scope");
    };
  }

  /**
   * Registers the Spring scope cleanup handler for both app and session lifecycles.
   *
   * @return the {@link SpringScopeCleanup}
   */
  @Bean
  SpringScopeCleanup webforjSpringScopeCleanup() {
    return new SpringScopeCleanup();
  }

  /**
   * Creates a default {@link TranslationResolver} bean if no other resolver is defined.
   *
   * <p>
   * Provides a default {@link BundleTranslationResolver} configured with the supported locales from
   * the application properties. Users can provide their own {@link TranslationResolver} bean (e.g.,
   * a database-backed resolver) to override this default.
   * </p>
   *
   * @param properties the Spring configuration properties
   * @return the default translation resolver
   */
  @Bean
  @ConditionalOnMissingBean(TranslationResolver.class)
  TranslationResolver translationResolver(SpringConfigurationProperties properties) {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    List<String> tags = null;
    if (properties.getI18n() != null) {
      tags = properties.getI18n().getSupportedLocales();
    }

    List<Locale> supportedLocales = LocaleUtils.parseLocaleTags(tags);

    logger.log(Logger.Level.DEBUG,
        "Creating default BundleTranslationResolver with {0} supported locales",
        supportedLocales.size());

    return new BundleTranslationResolver(supportedLocales, classLoader);
  }
}
