package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.springframework.mock.env.MockEnvironment;

class CompressionEnvironmentPostProcessorTest {

  private final CompressionEnvironmentPostProcessor processor =
      new CompressionEnvironmentPostProcessor();

  @Test
  void shouldEnableCompressionWhenTheApplicationLeavesItUnset() {
    MockEnvironment environment = new MockEnvironment();

    processor.postProcessEnvironment(environment, null);

    assertEquals("true", environment.getProperty("server.compression.enabled"));
  }

  @Test
  void shouldKeepCompressionOffWhenTheApplicationTurnsItOff() {
    MockEnvironment environment = new MockEnvironment();
    environment.setProperty("server.compression.enabled", "false");

    processor.postProcessEnvironment(environment, null);

    assertEquals("false", environment.getProperty("server.compression.enabled"));
  }
}
