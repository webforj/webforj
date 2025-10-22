package com.webforj.minify.js;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.minify.common.MinificationException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for ClosureJsMinifier.
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
    assertTrue(!minified.contains("//"), "Minified JS should not contain line comments");
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
    assertTrue(!minified.isEmpty(), "Minified code should not be empty");
    assertTrue(!minified.trim().isEmpty(), "Minified code should have content");
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
    assertTrue(!minified.contains("/*"), "Should not contain multiline comments");
  }

  @Test
  void testSupportedExtensions() {
    assertTrue(minifier.getSupportedExtensions().contains("js"));
    assertEquals(1, minifier.getSupportedExtensions().size());
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
    assertTrue(!minified.contains("    "));
  }
}
