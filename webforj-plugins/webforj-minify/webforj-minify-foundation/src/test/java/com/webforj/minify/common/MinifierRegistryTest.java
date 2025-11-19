package com.webforj.minify.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for MinifierRegistry.
 *
 * @author Kevin Hagel
 */
class MinifierRegistryTest {

  private MinifierRegistry registry;

  @BeforeEach
  void setUp() {
    registry = new MinifierRegistry();
  }

  @Test
  void testRegisterMinifier() {
    TestMinifier minifier = new TestMinifier();
    registry.register(minifier);

    assertEquals(1, registry.getMinifierCount());

    Optional<AssetMinifier> result = registry.getMinifier("test");
    assertTrue(result.isPresent());
    assertEquals(minifier, result.get());
  }

  @Test
  void testExtensionNormalization() {
    TestMinifier minifier = new TestMinifier();
    registry.register(minifier);

    // Should work with or without leading dot
    assertTrue(registry.getMinifier("test").isPresent());
    assertTrue(registry.getMinifier(".test").isPresent());
    assertTrue(registry.getMinifier("TEST").isPresent());
    assertTrue(registry.getMinifier(".TEST").isPresent());
  }

  @Test
  void testMultipleExtensions() {
    MultiExtensionMinifier minifier = new MultiExtensionMinifier();
    registry.register(minifier);

    // Should have 3 extensions registered
    assertEquals(3, registry.getMinifierCount());

    assertTrue(registry.getMinifier("foo").isPresent());
    assertTrue(registry.getMinifier("bar").isPresent());
    assertTrue(registry.getMinifier("baz").isPresent());
  }

  @Test
  void testUnknownExtension() {
    TestMinifier minifier = new TestMinifier();
    registry.register(minifier);

    Optional<AssetMinifier> result = registry.getMinifier("unknown");
    assertFalse(result.isPresent());
  }

  @Test
  void testLoadMinifiers() {
    // Load via SPI
    registry.loadMinifiers(getClass().getClassLoader());

    // The common module has no minifier implementations
    // CSS and JS minifiers are in separate modules (webforj-minify-css, webforj-minify-js)
    // This test verifies the SPI loading mechanism works without expecting specific minifiers
    int count = registry.getMinifierCount();
    assertEquals(0, count, "Common module should not contain any minifier implementations");
  }

  @Test
  void testMinifierReplacement() {
    TestMinifier minifier1 = new TestMinifier();
    TestMinifier minifier2 = new TestMinifier();

    registry.register(minifier1);
    registry.register(minifier2);

    // Should still be 1 minifier (replacement)
    assertEquals(1, registry.getMinifierCount());

    Optional<AssetMinifier> result = registry.getMinifier("test");
    assertTrue(result.isPresent());
    assertEquals(minifier2, result.get());
  }

  // Configuration Tests

  @Test
  void testConfigureMinifiersWithNullConfig() {
    ConfigurableMinifier minifier = new ConfigurableMinifier();
    registry.register(minifier);

    // Should not throw exception
    registry.configureMinifiers(null);

    // Minifier should not have been configured
    assertFalse(minifier.wasConfigured());
  }

  @Test
  void testConfigureMinifiersWithEmptyConfig() {
    ConfigurableMinifier minifier = new ConfigurableMinifier();
    registry.register(minifier);

    // Should not throw exception
    registry.configureMinifiers(Map.of());

    // Minifier should not have been configured (empty map is early exit)
    assertFalse(minifier.wasConfigured());
  }

  @Test
  void testConfigureMinifiersWithSingleMinifier() {
    ConfigurableMinifier minifier = new ConfigurableMinifier();
    registry.register(minifier);

    Map<String, Object> config = Map.of("configurable", Map.of("option", "value"));

    registry.configureMinifiers(config);

    assertTrue(minifier.wasConfigured());
    assertEquals(config, minifier.getReceivedConfig());
  }

  @Test
  void testConfigureMinifiersWithMultipleMinifiers() {
    ConfigurableMinifier minifier1 = new ConfigurableMinifier();
    ConfigurableMinifier minifier2 = new ConfigurableMinifier("config2");

    registry.register(minifier1);
    registry.register(minifier2);

    Map<String, Object> config =
        Map.of("configurable", Map.of("option1", "value1"), "config2", Map.of("option2", "value2"));

    registry.configureMinifiers(config);

    assertTrue(minifier1.wasConfigured());
    assertTrue(minifier2.wasConfigured());
    assertEquals(config, minifier1.getReceivedConfig());
    assertEquals(config, minifier2.getReceivedConfig());
  }

  @Test
  void testConfigureMinifiersWithException() {
    ConfigurableMinifier goodMinifier = new ConfigurableMinifier();
    FailingMinifier failingMinifier = new FailingMinifier();
    ConfigurableMinifier anotherGoodMinifier = new ConfigurableMinifier("config2");

    registry.register(goodMinifier);
    registry.register(failingMinifier);
    registry.register(anotherGoodMinifier);

    Map<String, Object> config = Map.of("configurable", Map.of("option", "value"));

    // Should not throw - should log warning and continue
    registry.configureMinifiers(config);

    // Good minifiers should still be configured
    assertTrue(goodMinifier.wasConfigured());
    assertTrue(anotherGoodMinifier.wasConfigured());
  }

  @Test
  void testConfigureMinifiersWithSpecificKeys() {
    ConfigurableMinifier minifier = new ConfigurableMinifier("myMinifier");
    registry.register(minifier);

    Map<String, Object> config = new java.util.HashMap<>();
    config.put("myMinifier", Map.of("compilationLevel", "ADVANCED", "prettyPrint", "true"));
    config.put("otherMinifier", Map.of("someOption", "someValue"));

    registry.configureMinifiers(config);

    assertTrue(minifier.wasConfigured());

    // Minifier receives entire config map - it extracts its own key
    assertEquals(config, minifier.getReceivedConfig());
  }

  /**
   * Test minifier implementation.
   */
  private static class TestMinifier implements AssetMinifier {
    @Override
    public String minify(String content, Path sourceFile) {
      return content.trim();
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of("test");
    }
  }

  /**
   * Test minifier with multiple extensions.
   */
  private static class MultiExtensionMinifier implements AssetMinifier {
    @Override
    public String minify(String content, Path sourceFile) {
      return content.trim();
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of("foo", "bar", "baz");
    }
  }

  /**
   * Test minifier that tracks configuration calls.
   */
  private static class ConfigurableMinifier implements AssetMinifier {
    private final String extension;
    private boolean configured = false;
    private java.util.Map<String, Object> receivedConfig;

    ConfigurableMinifier() {
      this("configurable");
    }

    ConfigurableMinifier(String extension) {
      this.extension = extension;
    }

    @Override
    public String minify(String content, Path sourceFile) {
      return content.trim();
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of(extension);
    }

    @Override
    public void configure(java.util.Map<String, Object> config) {
      this.configured = true;
      this.receivedConfig = config;
    }

    boolean wasConfigured() {
      return configured;
    }

    java.util.Map<String, Object> getReceivedConfig() {
      return receivedConfig;
    }
  }

  /**
   * Test minifier that throws exception during configuration.
   */
  private static class FailingMinifier implements AssetMinifier {
    @Override
    public String minify(String content, Path sourceFile) {
      return content.trim();
    }

    @Override
    public Set<String> getSupportedExtensions() {
      return Set.of("failing");
    }

    @Override
    public void configure(java.util.Map<String, Object> config) {
      throw new RuntimeException("Configuration failed");
    }
  }
}
