package com.webforj.devtools.craftforj.capabilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class CapabilitiesProviderTest {

  private static CapabilitiesProvider provider(String frameworkVersion, boolean licensed) {
    return provider(frameworkVersion, licensed, FeatureFlags.builder().build());
  }

  private static CapabilitiesProvider provider(String frameworkVersion, boolean licensed,
      FeatureFlags features) {
    return new CapabilitiesProvider(new VersionDetector("26.02"),
        new FrameworkVersionDetector(frameworkVersion), features, licensed);
  }

  @Nested
  @DisplayName("sourceCodeChanges")
  class SourceCodeChanges {

    @Test
    @DisplayName("Should be supported when licensed")
    void shouldBeSupported() {
      CapabilitiesProvider provider = provider("26.02", true);
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES));
    }

    @Test
    @DisplayName("Should not depend on the framework version, since craftforJ ships with it")
    void shouldNotDependOnFrameworkVersion() {
      CapabilitiesProvider provider = provider("26.01", true);
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES));
    }
  }

  @Nested
  @DisplayName("stylesheetChanges")
  class StylesheetChanges {

    @Test
    @DisplayName("Should be supported on any framework version when licensed")
    void shouldBeSupportedWhenLicensed() {
      CapabilitiesProvider provider = provider("26.01", true);
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES));
    }

    @Test
    @DisplayName("Should not be supported when unlicensed")
    void shouldNotBeSupportedWhenUnlicensed() {
      CapabilitiesProvider provider = provider("26.02", false);
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES));
    }
  }

  @Nested
  @DisplayName("feature flags")
  class Features {

    @Test
    @DisplayName("Should drop source capabilities when source changes are switched off")
    void shouldDropSourceCapabilities() {
      CapabilitiesProvider provider =
          provider("26.02", true, FeatureFlags.builder().sourceChanges(false).build());
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES));
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES));
    }

    @Test
    @DisplayName("Should drop only free form changes when the AI flag is switched off")
    void shouldDropFreeformOnly() {
      CapabilitiesProvider provider =
          provider("26.02", true, FeatureFlags.builder().aiFreeformChanges(false).build());
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES));
    }

    @Test
    @DisplayName("Should announce the assistant when licensed")
    void shouldAnnounceAssistant() {
      CapabilitiesProvider provider = provider("26.02", true);
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_AI_ASSISTANT));
    }

    @Test
    @DisplayName("Should drop the assistant and its free form edits when the AI is switched off")
    void shouldDropAssistant() {
      CapabilitiesProvider provider =
          provider("26.02", true, FeatureFlags.builder().aiEnabled(false).build());
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_AI_ASSISTANT));
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_FREEFORM_CHANGES));

      // The deterministic tooling keeps working without the assistant
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES));
    }

    @Test
    @DisplayName("Should drop stylesheet changes when switched off")
    void shouldDropStylesheetChanges() {
      CapabilitiesProvider provider =
          provider("26.02", true, FeatureFlags.builder().stylesheetChanges(false).build());
      assertFalse(provider.isSupported(CapabilitiesProvider.CAPABILITY_STYLESHEET_CHANGES));
      assertTrue(provider.isSupported(CapabilitiesProvider.CAPABILITY_SOURCE_CODE_CHANGES));
    }
  }

  @Nested
  @DisplayName("isFrameworkAtLeast")
  class IsAtLeast {

    @Test
    @DisplayName("Should report the framework version comparison")
    void shouldReportVersionComparison() {
      CapabilitiesProvider provider = provider("26.01", true);
      assertTrue(provider.isFrameworkAtLeast(26, 1));
      assertFalse(provider.isFrameworkAtLeast(26, 2));
    }
  }

  @Nested
  @DisplayName("license")
  class License {

    @ParameterizedTest
    @DisplayName("Should report license status matching input")
    @ValueSource(booleans = {true, false})
    void shouldReportLicenseStatus(boolean licensed) {
      CapabilitiesProvider provider = provider("26.02", licensed);
      assertEquals(licensed, provider.isLicensed());
    }

    @Test
    @DisplayName("Should return empty capabilities when unlicensed")
    void shouldReturnEmptyCapabilitiesWhenUnlicensed() {
      CapabilitiesProvider provider = provider("26.02", false);
      assertTrue(provider.getCapabilities().isEmpty());
    }

    @Test
    @DisplayName("Should return capabilities when licensed")
    void shouldReturnCapabilitiesWhenLicensed() {
      CapabilitiesProvider provider = provider("26.02", true);
      assertFalse(provider.getCapabilities().isEmpty());
    }
  }

  @Test
  @DisplayName("Should return unmodifiable capabilities list")
  void shouldReturnUnmodifiableList() {
    CapabilitiesProvider provider = provider("26.02", true);
    assertThrows(UnsupportedOperationException.class, () -> provider.getCapabilities().add("test"));
  }

  @Nested
  @DisplayName("hotswap")
  class Hotswap {

    @Test
    @DisplayName("Should report the state the build plugin declared for the run")
    void shouldReportTheDeclaredState() {
      System.setProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY, "hotswapAgent");
      System.setProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY, "limited");

      try {
        CapabilitiesProvider provider = provider("26.02", true);
        assertEquals("hotswapAgent", provider.getHotswapTool());
        assertEquals("limited", provider.getHotswapLevel());
      } finally {
        System.clearProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY);
        System.clearProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY);
      }
    }

    @Test
    @DisplayName("Should report nothing when no tool is attached")
    void shouldReportNothingWithoutTheDeclaration() {
      CapabilitiesProvider provider = provider("26.02", true);
      assertNull(provider.getHotswapTool());
      assertNull(provider.getHotswapLevel());
    }
  }
}
