package com.webforj.spring;

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
 * <p>
 * This configuration registers the {@link WebforjServlet} with the embedded servlet container. By
 * default, the servlet is mapped to "/" (the root), but this can be overridden by setting the
 * property {@code webforj.servlet.mapping} in the application configuration.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
@Configuration
@ConditionalOnClass(ServletContextInitializer.class)
@AutoConfigureBefore(WebMvcAutoConfiguration.class)
@EnableConfigurationProperties(WebforjConfigurationProperties.class)
public class WebforjAutoConfiguration {
  Logger logger = System.getLogger(WebforjAutoConfiguration.class.getName());

  /**
   * Registers the {@link WebforjServlet} with the Spring Boot embedded container.
   *
   * <p>
   * The servlet is mapped based on the {@code webforj.servlet.mapping} property. By default, this
   * is "/", but users can change it via their application configuration.
   * </p>
   *
   * @return the {@link ServletRegistrationBean} for the {@link WebforjServlet}
   */
  @Bean
  public ServletRegistrationBean<WebforjServlet> webforjServletRegistration(
      WebforjConfigurationProperties properties) {
    logger.log(Logger.Level.DEBUG,
        "Registering WebforjServlet with mapping " + properties.getServletMapping());

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
   * Injects the Spring {@link ApplicationContext} into the current {@link ContextHolder}.
   *
   * @return the {@link ContextInjector}
   */
  @Bean
  public ContextInjector webforjContextInjector() {
    return new ContextInjector();
  }
}
