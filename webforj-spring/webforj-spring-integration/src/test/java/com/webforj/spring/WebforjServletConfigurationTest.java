package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.web.servlet.HandlerMapping;
import org.springframework.web.servlet.mvc.Controller;
import org.springframework.web.servlet.mvc.ServletForwardingController;

class WebforjServletConfigurationTest {

  private WebforjServletConfiguration configuration;
  private SpringConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    configuration = new WebforjServletConfiguration();
    properties = new SpringConfigurationProperties();
  }

  @Test
  void shouldCreateWebforjRequestHandler() {
    List<String> excludeUrls = Arrays.asList("/api/**", "/static/**");
    properties.setExcludeUrls(excludeUrls);
    HandlerMapping resourceHandlerMapping = mock(HandlerMapping.class);

    WebforjRequestHandler handler =
        configuration.webforjRequestHandler(properties, resourceHandlerMapping);

    assertNotNull(handler);
    assertEquals(-1, handler.getOrder());
    assertTrue(handler.isPathExcluded("/api/test"));
    assertTrue(handler.isPathExcluded("/static/css/style.css"));
  }

  @Test
  void shouldCreateWebforjForwardingController() {
    Controller controller = configuration.webforjForwardingController();
    assertNotNull(controller);
    assertTrue(controller instanceof ServletForwardingController);
  }
}
