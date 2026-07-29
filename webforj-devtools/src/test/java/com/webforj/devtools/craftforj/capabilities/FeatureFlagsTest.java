package com.webforj.devtools.craftforj.capabilities;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class FeatureFlagsTest {

  @Nested
  @DisplayName("defaults")
  class Defaults {

    @Test
    @DisplayName("Should be on when the configuration is null")
    void shouldBeOnWhenConfigIsNull() {
      FeatureFlags flags = FeatureFlags.from(null);
      assertTrue(flags.isSourceChanges());
      assertTrue(flags.isStylesheetChanges());
      assertTrue(flags.isAiEnabled());
      assertTrue(flags.isAiFreeformChanges());
    }

    @Test
    @DisplayName("Should be on when the keys are absent")
    void shouldBeOnWhenKeysAbsent() {
      FeatureFlags flags = FeatureFlags.from(ConfigFactory.empty());
      assertTrue(flags.isSourceChanges());
      assertTrue(flags.isStylesheetChanges());
      assertTrue(flags.isAiEnabled());
      assertTrue(flags.isAiFreeformChanges());
    }

    @Test
    @DisplayName("Should be on when a key is explicitly null")
    void shouldBeOnWhenKeyIsNull() {
      Config config = ConfigFactory.parseString("webforj.devtools.craftforj.source-changes = null");
      assertTrue(FeatureFlags.from(config).isSourceChanges());
    }
  }

  @Nested
  @DisplayName("reading")
  class Reading {

    @Test
    @DisplayName("Should read each key from the configuration")
    void shouldReadEachKey() {
      Config config = ConfigFactory.parseString("""
          webforj.devtools.craftforj {
            source-changes = false
            stylesheet-changes = false
            ai.enabled = false
            ai.freeform-changes = false
          }
          """);

      FeatureFlags flags = FeatureFlags.from(config);
      assertFalse(flags.isSourceChanges());
      assertFalse(flags.isStylesheetChanges());
      assertFalse(flags.isAiEnabled());
      assertFalse(flags.isAiFreeformChanges());
    }

    @Test
    @DisplayName("Should read the AI switch on its own")
    void shouldReadAiEnabled() {
      Config config = ConfigFactory.parseString("webforj.devtools.craftforj.ai.enabled = false");

      FeatureFlags flags = FeatureFlags.from(config);
      assertFalse(flags.isAiEnabled());
      assertTrue(flags.isAiFreeformChanges());
      assertTrue(flags.isSourceChanges());
    }

    @Test
    @DisplayName("Should read the flags independently of one another")
    void shouldReadFlagsIndependently() {
      Config config =
          ConfigFactory.parseString("webforj.devtools.craftforj.ai.freeform-changes = false");

      FeatureFlags flags = FeatureFlags.from(config);
      assertTrue(flags.isSourceChanges());
      assertTrue(flags.isStylesheetChanges());
      assertFalse(flags.isAiFreeformChanges());
    }

    @Test
    @DisplayName("Should read flat keys as a Spring property source delivers them")
    void shouldReadFlatKeys() {
      Config config = ConfigFactory.parseMap(Map.of(FeatureFlags.KEY_SOURCE_CHANGES, "false",
          FeatureFlags.KEY_AI_FREEFORM_CHANGES, "false"));

      FeatureFlags flags = FeatureFlags.from(config);
      assertFalse(flags.isSourceChanges());
      assertFalse(flags.isAiFreeformChanges());
      assertTrue(flags.isStylesheetChanges());
    }
  }
}
