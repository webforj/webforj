package com.webforj.minify.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for MinifierRegistry.
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
}
