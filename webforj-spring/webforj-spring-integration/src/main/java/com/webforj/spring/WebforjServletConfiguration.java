package com.webforj.spring;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.mvc.Controller;
import org.springframework.web.servlet.mvc.ServletForwardingController;

/**
 * Configuration for webforJ servlet when mapped to the root context.
 *
 * <p>
 * This configuration is active only when webforJ is mapped to {@code /*}. It enables Spring MVC
 * integration by registering the webforJ servlet at an internal path ({@code /webforjServlet/*})
 * and setting up a request handler that manages forwarding between Spring MVC and webforJ.
 * </p>
 *
 * <p>
 * The handler forwards all HTTP methods (GET, POST, PUT, DELETE, etc.) to webforJ, except for URLs
 * matching patterns in {@code webforj.exclude-urls}, which are handled by Spring MVC.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@Configuration
@Conditional(ServletMappingCondition.class)
public class WebforjServletConfiguration {
  private static final Logger logger =
      System.getLogger(WebforjServletConfiguration.class.getName());

  /**
   * The internal servlet path where webforJ servlet is registered when root mapping is used.
   */
  public static final String WEBFORJ_SERVLET_MAPPING = "/webforjServlet/*";

  /**
   * The name of the webforJ servlet used for forwarding.
   */
  public static final String WEBFORJ_SERVLET_NAME = "webforjServlet";

  /**
   * Creates a request handler that manages forwarding to webforJ servlet.
   *
   * <p>
   * This handler checks incoming requests against the excluded URL patterns defined in
   * {@code webforj.exclude-urls}. Requests matching excluded patterns are passed to Spring MVC
   * controllers, while all other requests (regardless of HTTP method) are forwarded to the webforJ
   * servlet.
   * </p>
   *
   * @param properties the Spring configuration properties containing excluded URLs
   * @param resourceHandlerMapping the Spring resource handler mapping, if available
   *
   * @return the configured request handler
   */
  // @formatter:off
  @Bean
  WebforjRequestHandler webforjRequestHandler(
      SpringConfigurationProperties properties,
      @Autowired(required = false)
      @Qualifier("resourceHandlerMapping")
      HandlerMapping resourceHandlerMapping
  ) {
    // @formatter:on

    logger.log(Level.DEBUG, "Creating webforJ request handler for root mapping");

    return new WebforjRequestHandler(properties.getExcludeUrls(), webforjForwardingController(),
        resourceHandlerMapping);
  }

  /**
   * Creates a forwarding controller that forwards requests to the webforJ servlet.
   *
   * @return the configured forwarding controller
   */
  @Bean
  Controller webforjForwardingController() {
    ServletForwardingController controller = new ServletForwardingController();
    controller.setServletName(WEBFORJ_SERVLET_NAME);
    logger.log(Level.DEBUG, "Created forwarding controller for servlet: " + WEBFORJ_SERVLET_NAME);

    return controller;
  }
}
