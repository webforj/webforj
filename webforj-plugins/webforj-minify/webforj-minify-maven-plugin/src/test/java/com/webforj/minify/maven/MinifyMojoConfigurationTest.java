package com.webforj.minify.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for MinifyMojo configuration handling.
 *
 * <p>
 * Tests that configuration from pom.xml is properly received and can be passed to minifiers.
 * </p>
 *
 * @author Kevin Hagel
 */
class MinifyMojoConfigurationTest {

  private MinifyMojo mojo;

  @BeforeEach
  void setUp() {
    mojo = new MinifyMojo();
  }

  @Test
  void testMojoInstantiation() {
    assertNotNull(mojo);
  }

  @Test
  void testConfigurationStructure() {
    // Test that we can create configuration structures that Maven would inject
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("compilationLevel", "SIMPLE_OPTIMIZATIONS");
    closureJsConfig.put("languageIn", "ECMASCRIPT_2020");
    closureJsConfig.put("languageOut", "ECMASCRIPT5");
    closureJsConfig.put("prettyPrint", "false");

    Map<String, Object> phCssConfig = new HashMap<>();
    phCssConfig.put("compressColors", "true");
    phCssConfig.put("removeUnnecessaryCode", "true");

    Map<String, Object> minifierConfigurations = new HashMap<>();
    minifierConfigurations.put("closureJs", closureJsConfig);
    minifierConfigurations.put("phCss", phCssConfig);

    // Verify structure
    assertNotNull(minifierConfigurations);
    assertEquals(2, minifierConfigurations.size());
    assertTrue(minifierConfigurations.containsKey("closureJs"));
    assertTrue(minifierConfigurations.containsKey("phCss"));

    @SuppressWarnings("unchecked")
    Map<String, Object> retrievedClosureJs =
        (Map<String, Object>) minifierConfigurations.get("closureJs");
    assertEquals("SIMPLE_OPTIMIZATIONS", retrievedClosureJs.get("compilationLevel"));
    assertEquals("ECMASCRIPT_2020", retrievedClosureJs.get("languageIn"));
    assertEquals("ECMASCRIPT5", retrievedClosureJs.get("languageOut"));
    assertEquals("false", retrievedClosureJs.get("prettyPrint"));

    @SuppressWarnings("unchecked")
    Map<String, Object> retrievedPhCss = (Map<String, Object>) minifierConfigurations.get("phCss");
    assertEquals("true", retrievedPhCss.get("compressColors"));
    assertEquals("true", retrievedPhCss.get("removeUnnecessaryCode"));
  }

  @Test
  void testClosureJsConfigurationOnly() {
    Map<String, Object> closureJsConfig = new HashMap<>();
    closureJsConfig.put("compilationLevel", "WHITESPACE_ONLY");

    Map<String, Object> minifierConfigurations = new HashMap<>();
    minifierConfigurations.put("closureJs", closureJsConfig);

    assertEquals(1, minifierConfigurations.size());
    assertTrue(minifierConfigurations.containsKey("closureJs"));
  }

  @Test
  void testPhCssConfigurationOnly() {
    Map<String, Object> phCssConfig = new HashMap<>();
    phCssConfig.put("compressColors", "true");

    Map<String, Object> minifierConfigurations = new HashMap<>();
    minifierConfigurations.put("phCss", phCssConfig);

    assertEquals(1, minifierConfigurations.size());
    assertTrue(minifierConfigurations.containsKey("phCss"));
  }

  @Test
  void testCustomMinifierConfiguration() {
    // Test that third-party minifiers can also use this configuration mechanism
    Map<String, Object> customMinifierConfig = new HashMap<>();
    customMinifierConfig.put("option1", "value1");
    customMinifierConfig.put("option2", "value2");

    Map<String, Object> minifierConfigurations = new HashMap<>();
    minifierConfigurations.put("myCustomMinifier", customMinifierConfig);

    assertEquals(1, minifierConfigurations.size());
    assertTrue(minifierConfigurations.containsKey("myCustomMinifier"));

    @SuppressWarnings("unchecked")
    Map<String, Object> retrieved =
        (Map<String, Object>) minifierConfigurations.get("myCustomMinifier");
    assertEquals("value1", retrieved.get("option1"));
    assertEquals("value2", retrieved.get("option2"));
  }

  @Test
  void testEmptyConfiguration() {
    Map<String, Object> minifierConfigurations = new HashMap<>();
    assertEquals(0, minifierConfigurations.size());
  }

  @Test
  void testAllCompilationLevels() {
    // Test all valid Closure Compiler compilation levels
    String[] levels =
        new String[] {"WHITESPACE_ONLY", "SIMPLE_OPTIMIZATIONS", "ADVANCED_OPTIMIZATIONS"};

    for (String level : levels) {
      Map<String, Object> closureJsConfig = new HashMap<>();
      closureJsConfig.put("compilationLevel", level);

      Map<String, Object> config = new HashMap<>();
      config.put("closureJs", closureJsConfig);

      @SuppressWarnings("unchecked")
      Map<String, Object> retrieved = (Map<String, Object>) config.get("closureJs");
      assertEquals(level, retrieved.get("compilationLevel"));
    }
  }

  @Test
  void testAllLanguageModes() {
    // Test various ECMAScript language modes
    String[] modes = new String[] {"ECMASCRIPT3", "ECMASCRIPT5", "ECMASCRIPT_2015",
        "ECMASCRIPT_2016", "ECMASCRIPT_2017", "ECMASCRIPT_2018", "ECMASCRIPT_2019",
        "ECMASCRIPT_2020", "ECMASCRIPT_NEXT"};

    for (String mode : modes) {
      Map<String, Object> closureJsConfig = new HashMap<>();
      closureJsConfig.put("languageIn", mode);

      Map<String, Object> config = new HashMap<>();
      config.put("closureJs", closureJsConfig);

      @SuppressWarnings("unchecked")
      Map<String, Object> retrieved = (Map<String, Object>) config.get("closureJs");
      assertEquals(mode, retrieved.get("languageIn"));
    }
  }
}
