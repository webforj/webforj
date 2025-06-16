package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class WebforjConfigurationPropertiesTest {

  private WebforjConfigurationProperties properties;

  @BeforeEach
  void setUp() {
    properties = new WebforjConfigurationProperties();
  }

  @ParameterizedTest
  @ValueSource(strings = {"/", "/*", "/webforj/*", "/app/*", "/api/webforj/*"})
  void shouldHandleVariousServletMappingPatterns(String mapping) {
    properties.setServletMapping(mapping);
    assertEquals(mapping, properties.getServletMapping());
  }
}
