package com.webforj.spring;

import java.util.Map;
import org.springframework.boot.EnvironmentPostProcessor;
import org.springframework.boot.SpringApplication;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;

/**
 * Turns on response compression of the embedded server unless the application configures it.
 *
 * <p>
 * The messages the server sends to the browser and the client library are text that compresses to
 * about a sixth of its size. The default is added with the lowest precedence, so any
 * {@code server.compression.enabled} value the application sets wins, and the mime types and the
 * minimum response size stay at the defaults of Spring Boot unless the application sets them.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
public class CompressionEnvironmentPostProcessor implements EnvironmentPostProcessor {
  private static final String PROPERTY_SOURCE_NAME = "webforj-compression-defaults";
  private static final String ENABLED = "server.compression.enabled";

  @Override
  public void postProcessEnvironment(ConfigurableEnvironment environment,
      SpringApplication application) {
    environment.getPropertySources()
        .addLast(new MapPropertySource(PROPERTY_SOURCE_NAME, Map.of(ENABLED, "true")));
  }
}
