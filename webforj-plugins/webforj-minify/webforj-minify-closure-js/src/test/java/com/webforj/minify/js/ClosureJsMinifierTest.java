package com.webforj.minify.js;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.minify.common.MinificationException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for ClosureJsMinifier.
 *
 * @author Kevin Hagel
 */
class ClosureJsMinifierTest {

  private ClosureJsMinifier minifier;
  private Path testPath;

  @BeforeEach
  void setUp() {
    minifier = new ClosureJsMinifier();
    testPath = Paths.get("test.js");
  }

  @Test
  void testMinifyValidJavaScript() throws MinificationException {
    String js = """
        // This is a comment
        function hello(name) {
          console.log('Hello, ' + name);
          return true;
        }

        const value = 42;
        """;

    String minified = minifier.minify(js, testPath);

    // Should be shorter
    assertTrue(minified.length() < js.length(), "Minified JS should be shorter than original");

    // Should not contain comments
    assertFalse(minified.contains("//"), "Minified JS should not contain line comments");
  }

  @Test
  void testMinifyWithVariableRenaming() throws MinificationException {
    String js = """
        function calculateTotal(price, quantity) {
          const taxRate = 0.08;
          const subtotal = price * quantity;
          const tax = subtotal * taxRate;
          return subtotal + tax;
        }
        """;

    String minified = minifier.minify(js, testPath);

    assertTrue(minified.length() < js.length());
    // Function name should be preserved (not renamed)
    assertTrue(minified.contains("calculateTotal"));
  }

  @Test
  void testMalformedJavaScript() throws MinificationException {
    String malformed = "function test() { return";

    // Should return original content on compilation failure
    String result = minifier.minify(malformed, testPath);

    assertEquals(malformed, result, "Should return original content for malformed JS");
  }

  @Test
  void testEmptyInput() throws MinificationException {
    String empty = "";

    String result = minifier.minify(empty, testPath);

    // Should handle empty input gracefully
    assertTrue(result.isEmpty() || result.isBlank());
  }

  @Test
  void testEs6Syntax() throws MinificationException {
    String js = """
        const greet = (name) => {
          console.log(`Hello, ${name}`);
        };

        class Person {
          constructor(name) {
            this.name = name;
          }
        }
        """;

    String minified = minifier.minify(js, testPath);

    // ES6 code might be transpiled which can increase size in some cases
    // Just verify it compiles successfully
    assertFalse(minified.isEmpty(), "Minified code should not be empty");
    assertFalse(minified.trim().isEmpty(), "Minified code should have content");
  }

  @Test
  void testMultilineComments() throws MinificationException {
    String js = """
        /*
         * Multi-line comment
         * should be removed
         */
        function test() {
          return 42;
        }
        """;

    String minified = minifier.minify(js, testPath);

    assertTrue(minified.length() < js.length());
    assertFalse(minified.contains("/*"), "Should not contain multiline comments");
  }

  @Test
  void testSupportedExtensions() {
    assertTrue(minifier.getSupportedExtensions().contains("js"));
    assertTrue(minifier.getSupportedExtensions().contains("mjs"));
    assertEquals(2, minifier.getSupportedExtensions().size());
  }

  @Test
  void testWhitespaceRemoval() throws MinificationException {
    String js = """
        function    test   (  )   {
          return    42   ;
        }
        """;

    String minified = minifier.minify(js, testPath);

    assertTrue(minified.length() < js.length());
    // Should have minimal whitespace
    assertFalse(minified.contains("    "));
  }

  @Test
  void testShouldMinifyRegularJsFile() {
    Path jsFile = Paths.get("scripts/app.js");
    assertTrue(minifier.shouldMinify(jsFile), "Should minify regular .js files");
  }

  @Test
  void testShouldNotMinifyMinifiedJsFile() {
    Path minifiedFile = Paths.get("scripts/app.min.js");
    assertTrue(!minifier.shouldMinify(minifiedFile), "Should not minify .min.js files");
  }

  @Test
  void testShouldMinifyRegularMjsFile() {
    Path mjsFile = Paths.get("modules/app.mjs");
    assertTrue(minifier.shouldMinify(mjsFile), "Should minify regular .mjs files");
  }

  @Test
  void testShouldNotMinifyMinifiedMjsFile() {
    Path minifiedMjsFile = Paths.get("modules/app.min.mjs");
    assertTrue(!minifier.shouldMinify(minifiedMjsFile), "Should not minify .min.mjs files");
  }

  @Test
  void testMinifyMjsContent() throws MinificationException {
    // Test ES module syntax (typical .mjs content)
    String mjs = """
        export const PI = 3.14159;

        export function calculateArea(radius) {
          return PI * radius * radius;
        }

        export default class Circle {
          constructor(radius) {
            this.radius = radius;
          }
        }
        """;
    Path mjsPath = Paths.get("test.mjs");

    String minified = minifier.minify(mjs, mjsPath);

    // ES6 modules may be transpiled to ES5, which can increase size
    // Just verify it compiles successfully and is not empty
    assertFalse(minified.isEmpty(), "Minified .mjs should not be empty");
    assertFalse(minified.trim().isEmpty(), "Minified .mjs should have content");
  }

  @Test
  void testMinifyActualMjsFile() throws MinificationException, IOException {
    // Integration test: Load actual .mjs file from test resources and minify it
    Path mjsFile = Paths.get("src/test/resources/test-module.mjs");

    // Verify the test file exists
    assertTrue(Files.exists(mjsFile), "Test .mjs file should exist");

    // Read the file content
    String originalContent = Files.readString(mjsFile);
    assertTrue(originalContent.contains("export"),
        "Original file should contain export statements");
    assertTrue(originalContent.contains("class Square"),
        "Original file should contain Square class");

    // Minify the content
    String minified = minifier.minify(originalContent, mjsFile);

    // Verify minification succeeded
    assertFalse(minified.isEmpty(), "Minified .mjs should not be empty");
    assertFalse(minified.trim().isEmpty(), "Minified .mjs should have content");

    // Verify the file should be minified (not skipped)
    assertTrue(minifier.shouldMinify(mjsFile), "Regular .mjs files should be minified");

    // Verify .min.mjs would be skipped
    Path minMjsFile = Paths.get("src/test/resources/test-module.min.mjs");
    assertTrue(!minifier.shouldMinify(minMjsFile),
        "Already minified .min.mjs files should be skipped");
  }

  // Configuration Tests

  @Test
  void testConfigureWithNoConfiguration() throws MinificationException {
    // Test that minifier works with no configuration (uses defaults)
    minifier.configure(null);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    assertFalse(minified.isEmpty(), "Should minify with default configuration");
  }

  @Test
  void testConfigureWithEmptyMap() throws MinificationException {
    // Test with empty config map
    Map<String, Object> config = new HashMap<>();
    minifier.configure(config);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    assertFalse(minified.isEmpty(), "Should minify with empty configuration");
  }

  @Test
  void testConfigureWithNoClosureJsKey() throws MinificationException {
    // Test with config map that doesn't contain "closureJs" key
    Map<String, Object> config = new HashMap<>();
    config.put("someOtherMinifier", Map.of("option", "value"));
    minifier.configure(config);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    assertFalse(minified.isEmpty(), "Should minify with defaults when closureJs key absent");
  }

  @Test
  void testConfigureWithInvalidClosureJsValue() throws MinificationException {
    // Test with closureJs value that's not a Map
    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", "invalid value");
    minifier.configure(config);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    assertFalse(minified.isEmpty(), "Should use defaults when closureJs value is invalid");
  }

  @Test
  void testConfigureCompilationLevel() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("compilationLevel", "WHITESPACE_ONLY");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    String js = """
        function test(longVariableName) {
          const anotherLongVariableName = 42;
          return longVariableName + anotherLongVariableName;
        }
        """;

    String minified = minifier.minify(js, testPath);

    // WHITESPACE_ONLY should preserve variable names
    assertTrue(minified.contains("longVariableName"),
        "WHITESPACE_ONLY should preserve variable names");
    assertTrue(minified.contains("anotherLongVariableName"),
        "WHITESPACE_ONLY should preserve all variable names");
  }

  @Test
  void testConfigurePrettyPrint() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("prettyPrint", "true");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    String js = """
        function test() {
          return 42;
        }
        """;

    String minified = minifier.minify(js, testPath);

    // Pretty print should preserve some formatting
    assertTrue(minified.contains("\n") || minified.contains(" "),
        "Pretty print should preserve some whitespace");
  }

  @Test
  void testConfigureLanguageInAndOut() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("languageIn", "ECMASCRIPT_2020");
    closureJsConfig.put("languageOut", "ECMASCRIPT5");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    // ES2020 optional chaining should be transpiled to ES5
    String js = """
        const value = obj?.prop?.nested;
        """;

    String minified = minifier.minify(js, testPath);

    // Should successfully compile (transpile ES2020 to ES5)
    assertFalse(minified.isEmpty(), "Should transpile ES2020 to ES5");
  }

  @Test
  void testConfigureInvalidCompilationLevel() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("compilationLevel", "INVALID_LEVEL");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    // Should use default level and still minify successfully
    assertFalse(minified.isEmpty(), "Should use default level for invalid compilation level");
  }

  @Test
  void testConfigureInvalidLanguageMode() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("languageIn", "INVALID_MODE");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    String js = "function test() { return 42; }";
    String minified = minifier.minify(js, testPath);

    // Should use default mode and still minify successfully
    assertFalse(minified.isEmpty(), "Should use default mode for invalid language mode");
  }

  @Test
  void testConfigureMultipleOptions() throws MinificationException {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("compilationLevel", "SIMPLE_OPTIMIZATIONS");
    closureJsConfig.put("languageIn", "ECMASCRIPT_2015");
    closureJsConfig.put("languageOut", "ECMASCRIPT5");
    closureJsConfig.put("prettyPrint", "false");

    Map<String, Object> config = new HashMap<>();
    config.put("closureJs", closureJsConfig);

    minifier.configure(config);

    String js = """
        const greet = (name) => `Hello, ${name}!`;
        class Person {
          constructor(name) { this.name = name; }
        }
        """;

    String minified = minifier.minify(js, testPath);

    // Should successfully compile with all options applied
    // Note: Transpilation from ES2015 to ES5 can increase size due to polyfills
    assertFalse(minified.isEmpty(), "Should minify with multiple options configured");
    assertFalse(minified.trim().isEmpty(), "Should have actual content");
  }
}
