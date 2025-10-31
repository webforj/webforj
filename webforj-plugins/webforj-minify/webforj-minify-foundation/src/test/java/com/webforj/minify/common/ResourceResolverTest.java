package com.webforj.minify.common;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Unit tests for ResourceResolver.
 *
 * @author Kevin Hagel
 */
class ResourceResolverTest {

  @TempDir
  Path tempDir;

  private ResourceResolver resolver;

  @BeforeEach
  void setUp() {
    resolver = new ResourceResolver(tempDir);
  }

  @Test
  void testWsProtocol() throws IOException {
    Path staticDir = tempDir.resolve("static");
    Files.createDirectories(staticDir);
    Path jsDir = staticDir.resolve("js");
    Files.createDirectories(jsDir);

    Path resolved = resolver.resolve("ws://js/test.js");

    assertEquals(jsDir.resolve("test.js").toAbsolutePath().normalize(), resolved);
  }

  @Test
  void testContextProtocol() throws IOException {
    Path cssDir = tempDir.resolve("css");
    Files.createDirectories(cssDir);

    Path resolved = resolver.resolve("context://css/test.css");

    assertEquals(cssDir.resolve("test.css").toAbsolutePath().normalize(), resolved);
  }

  @Test
  void testNoProtocol() {
    // URLs without protocols should be rejected with clear error message
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
      resolver.resolve("css/test.css");
    });

    assertTrue(exception.getMessage().contains("URL without protocol cannot be resolved"));
    assertTrue(exception.getMessage().contains("Use ws:// or context://"));
  }

  @Test
  void testDirectoryTraversalPrevention() {
    // Attempt to escape resources root
    SecurityException exception = assertThrows(SecurityException.class, () -> {
      resolver.resolve("context://../../../etc/passwd");
    });

    assertTrue(exception.getMessage().contains("Path traversal detected"));
  }

  @Test
  void testDirectoryTraversalWithWs() {
    // Attempt to escape via ws protocol
    SecurityException exception = assertThrows(SecurityException.class, () -> {
      resolver.resolve("ws://../../../etc/passwd");
    });

    assertTrue(exception.getMessage().contains("Path traversal detected"));
  }

  @Test
  void testUnknownProtocol() {
    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
      resolver.resolve("unknown://test.css");
    });

    assertTrue(exception.getMessage().contains("Unknown protocol"));
  }

  @Test
  void testNestedDirectories() throws IOException {
    Path staticDir = tempDir.resolve("static");
    Files.createDirectories(staticDir);
    Path nestedDir = staticDir.resolve("components").resolve("buttons");
    Files.createDirectories(nestedDir);

    Path resolved = resolver.resolve("ws://components/buttons/style.css");

    assertEquals(nestedDir.resolve("style.css").toAbsolutePath().normalize(), resolved);
  }
}
