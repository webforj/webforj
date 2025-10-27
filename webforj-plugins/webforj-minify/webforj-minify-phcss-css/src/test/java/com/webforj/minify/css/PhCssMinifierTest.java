package com.webforj.minify.css;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.minify.common.MinificationException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for PhCssMinifier.
 */
class PhCssMinifierTest {

  private PhCssMinifier minifier;
  private Path testPath;

  @BeforeEach
  void setUp() {
    minifier = new PhCssMinifier();
    testPath = Paths.get("test.css");
  }

  @Test
  void testMinifyValidCss() throws MinificationException {
    String css = """
        /* Comment */
        body {
          margin: 0;
          padding: 0;
        }

        .container {
          max-width: 1200px;
        }
        """;

    String minified = minifier.minify(css, testPath);

    // Should be shorter
    assertTrue(minified.length() < css.length(), "Minified CSS should be shorter than original");

    // Should not contain comments
    assertTrue(!minified.contains("/*"), "Minified CSS should not contain comments");

    // Should still contain the actual rules
    assertTrue(minified.contains("body"), "Should contain body selector");
    assertTrue(minified.contains("margin"), "Should contain margin property");
  }

  @Test
  void testMinifyWithoutSpaces() throws MinificationException {
    String css = """
        body {
          margin: 0;
          padding: 10px 20px;
        }
        """;

    String minified = minifier.minify(css, testPath);

    // Should have minimal whitespace
    assertTrue(minified.length() < css.length());
    assertNotEquals(css.trim(), minified);
  }

  @Test
  void testMalformedCss() throws MinificationException {
    String malformed = "body { margin: 0";

    // Should return original content on parse failure
    String result = minifier.minify(malformed, testPath);

    assertEquals(malformed, result, "Should return original content for malformed CSS");
  }

  @Test
  void testEmptyInput() throws MinificationException {
    String empty = "";

    String result = minifier.minify(empty, testPath);

    // Should handle empty input gracefully
    assertTrue(result.isEmpty() || result.isBlank());
  }

  @Test
  void testComplexSelectors() throws MinificationException {
    String css = """
        .container > div:first-child {
          margin-top: 0;
        }

        @media (max-width: 768px) {
          .container {
            padding: 10px;
          }
        }
        """;

    String minified = minifier.minify(css, testPath);

    assertTrue(minified.length() < css.length());
    assertTrue(minified.contains(".container"));
    assertTrue(minified.contains("@media"));
  }

  @Test
  void testSupportedExtensions() {
    assertTrue(minifier.getSupportedExtensions().contains("css"));
    assertEquals(1, minifier.getSupportedExtensions().size());
  }

  @Test
  void testShouldMinifyRegularCssFile() {
    Path cssFile = Paths.get("styles/app.css");
    assertTrue(minifier.shouldMinify(cssFile), "Should minify regular .css files");
  }

  @Test
  void testShouldNotMinifyMinifiedCssFile() {
    Path minifiedFile = Paths.get("styles/app.min.css");
    assertTrue(!minifier.shouldMinify(minifiedFile), "Should not minify .min.css files");
  }
}
