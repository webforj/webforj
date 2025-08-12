package com.webforj.spring;

import com.typesafe.config.Config;
import com.webforj.servlet.WebforjServlet;
import java.lang.System.Logger;
import org.springframework.boot.autoconfigure.AutoConfigureBefore;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.web.servlet.WebMvcAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.ServletContextInitializer;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

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
   * The servlet is mapped based on the {@code webforj.servletMapping} property. By default, this is
   * "/", but users can change it via their application configuration.
   * </p>
   *
   * @return the {@link ServletRegistrationBean} for the {@link WebforjServlet}
   */
  @Bean
  ServletRegistrationBean<WebforjServlet> webforjServletRegistration(
      SpringConfigurationProperties properties, Config webforjConfig) {
    logger.log(Logger.Level.DEBUG,
        "Registering WebforjServlet with mapping " + properties.getServletMapping());

    // Set the configuration for the servlet
    WebforjServlet.setConfig(webforjConfig);
    logger.log(Logger.Level.DEBUG, "Set webforj configuration for servlet");

    WebforjServlet webforjServlet = new WebforjServlet();

    ServletRegistrationBean<WebforjServlet> registrationBean =
        new ServletRegistrationBean<>(webforjServlet, properties.getServletMapping());
    logger.log(Logger.Level.DEBUG,
        "Registered WebforjServlet with mapping " + properties.getServletMapping());

    registrationBean.setLoadOnStartup(1);
    logger.log(Logger.Level.DEBUG, "Setting load-on-startup to 1.");
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
}
