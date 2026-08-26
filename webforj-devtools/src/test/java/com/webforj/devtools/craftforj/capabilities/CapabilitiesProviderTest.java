package com.webforj.devtools.craftforj.capabilities;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.ai.AiAssistantCapability;
import com.webforj.devtools.craftforj.inspector.source.SourceChangesCapability;
import com.webforj.devtools.craftforj.inspector.source.SourceFreeformChangesCapability;
import com.webforj.devtools.craftforj.styles.StylesheetChangesCapability;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

@DisplayName("CapabilitiesProvider")
class CapabilitiesProviderTest {

  @Nested
  @DisplayName("license gate")
  class LicenseGate {

    @Test
    @DisplayName("Should announce nothing when unlicensed")
    void shouldAnnounceNothingWhenUnlicensed() {
      App app = mock(App.class);
      List<App> seen = new ArrayList<>();

      CapabilitiesProvider provider =
          buildProvider(app, false, List.of(createRecordingFake("alpha", seen, true)));

      assertTrue(provider.getCapabilities().isEmpty());
      assertFalse(provider.isSupported("alpha"));
      assertTrue(seen.isEmpty());
    }

    @Test
    @DisplayName("Should hand out an unmodifiable list when unlicensed")
    void shouldHandOutUnmodifiableListWhenUnlicensed() {
      CapabilitiesProvider provider =
          buildProvider(mock(App.class), false, List.of(createFake("alpha", true)));

      assertThrows(UnsupportedOperationException.class,
          () -> provider.getCapabilities().add("alpha"));
    }

    @Test
    @DisplayName("Should announce the supported keys in declaration order when licensed")
    void shouldAnnounceSupportedKeysInDeclarationOrder() {
      List<CraftforjCapability> declared =
          List.of(createFake("alpha", true), createFake("beta", false), createFake("gamma", true));

      CapabilitiesProvider provider = buildProvider(mock(App.class), true, declared);

      assertEquals(List.of("alpha", "gamma"), provider.getCapabilities());
      assertTrue(provider.isSupported("alpha"));
      assertFalse(provider.isSupported("beta"));
      assertTrue(provider.isSupported("gamma"));
    }

    @Test
    @DisplayName("Should hand out an unmodifiable list when licensed")
    void shouldHandOutUnmodifiableListWhenLicensed() {
      CapabilitiesProvider provider =
          buildProvider(mock(App.class), true, List.of(createFake("alpha", true)));

      assertThrows(UnsupportedOperationException.class,
          () -> provider.getCapabilities().add("beta"));
    }

    @Test
    @DisplayName("Should pass the same application to every check")
    void shouldPassSameApplicationToEveryCheck() {
      App app = mock(App.class);
      List<App> seen = new ArrayList<>();
      List<CraftforjCapability> declared = List.of(createRecordingFake("alpha", seen, true),
          createRecordingFake("beta", seen, false));

      buildProvider(app, true, declared);

      assertEquals(2, seen.size());
      assertSame(app, seen.get(0));
      assertSame(app, seen.get(1));
    }

    @Test
    @DisplayName("Should reject a key declared twice, naming the key")
    void shouldRejectKeyDeclaredTwice() {
      List<CraftforjCapability> declared =
          List.of(createFake("alpha", true), createFake("alpha", false));

      IllegalStateException thrown = assertThrows(IllegalStateException.class,
          () -> buildProvider(mock(App.class), true, declared));

      assertTrue(thrown.getMessage().contains("alpha"));
    }
  }

  @Nested
  @DisplayName("runtime facts")
  class RuntimeFacts {

    @Test
    @DisplayName("Should report the craftforJ version and the license flag")
    void shouldReportVersionAndLicenseFlag() {
      assertEquals("26.02", buildProvider(mock(App.class), true, List.of()).getVersion());
      assertTrue(buildProvider(mock(App.class), true, List.of()).isLicensed());
      assertFalse(buildProvider(mock(App.class), false, List.of()).isLicensed());
    }

    @Test
    @DisplayName("Should compare the framework version against the requirement")
    void shouldCompareFrameworkVersion() {
      CapabilitiesProvider provider = buildProvider(mock(App.class), true, List.of());

      assertTrue(provider.isFrameworkAtLeast(26, 1));
      assertFalse(provider.isFrameworkAtLeast(26, 3));
    }

    @Test
    @DisplayName("Should report a full compile gate on a runtime carrying a compiler")
    void shouldReportFullCompileGate() {
      assertEquals(CapabilitiesProvider.COMPILE_GATE_FULL,
          buildProvider(mock(App.class), true, List.of()).getCompileGate());
    }

    @Test
    @DisplayName("Should read the hotswap tool from the system property")
    void shouldReadHotswapTool() {
      CapabilitiesProvider provider = buildProvider(mock(App.class), true, List.of());
      String previous = System.getProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY);

      try {
        System.clearProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY);
        assertNull(provider.getHotswapTool());

        System.setProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY, "hotswapAgent");
        assertEquals("hotswapAgent", provider.getHotswapTool());
      } finally {
        setOrClearProperty(CapabilitiesProvider.HOTSWAP_TOOL_PROPERTY, previous);
      }
    }

    @Test
    @DisplayName("Should read the hotswap level from the system property")
    void shouldReadHotswapLevel() {
      CapabilitiesProvider provider = buildProvider(mock(App.class), true, List.of());
      String previous = System.getProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY);

      try {
        System.clearProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY);
        assertNull(provider.getHotswapLevel());

        System.setProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY, "limited");
        assertEquals("limited", provider.getHotswapLevel());
      } finally {
        setOrClearProperty(CapabilitiesProvider.HOTSWAP_LEVEL_PROPERTY, previous);
      }
    }
  }

  @Nested
  @DisplayName("declared capabilities")
  class DeclaredCapabilities {

    @Test
    @DisplayName("Should load the declared services in file order")
    void shouldLoadDeclaredServicesInFileOrder() {
      List<CraftforjCapability> declared = CapabilitiesProvider.loadCapabilities();

      assertEquals(4, declared.size());
      assertEquals(SourceChangesCapability.class, declared.get(0).getClass());
      assertEquals(StylesheetChangesCapability.class, declared.get(1).getClass());
      assertEquals(AiAssistantCapability.class, declared.get(2).getClass());
      assertEquals(SourceFreeformChangesCapability.class, declared.get(3).getClass());
    }

    @Test
    @DisplayName("Should announce every declared capability on an empty configuration")
    void shouldAnnounceEveryCapabilityOnEmptyConfiguration() {
      assertEquals(
          List.of(SourceChangesCapability.KEY, StylesheetChangesCapability.KEY,
              AiAssistantCapability.KEY, SourceFreeformChangesCapability.KEY),
          getAnnouncedFor(mock(App.class), ""));
    }

    @Test
    @DisplayName("Should announce every declared capability without an environment")
    void shouldAnnounceEveryCapabilityWithoutEnvironment() {
      try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
        mocked.when(Environment::getCurrent).thenReturn(null);

        assertEquals(
            List.of(SourceChangesCapability.KEY, StylesheetChangesCapability.KEY,
                AiAssistantCapability.KEY, SourceFreeformChangesCapability.KEY),
            new CapabilitiesProvider(mock(App.class), true).getCapabilities());
      }
    }

    @Test
    @DisplayName("Should drop the assistant and the freeform changes when the assistant is off")
    void shouldDropAssistantAndFreeformWhenAssistantOff() {
      String hocon = AiAssistantCapability.CONFIG_KEY + " = false";

      assertEquals(List.of(SourceChangesCapability.KEY, StylesheetChangesCapability.KEY),
          getAnnouncedFor(mock(App.class), hocon));
    }

    @Test
    @DisplayName("Should drop the source and the freeform changes when source changes are off")
    void shouldDropSourceAndFreeformWhenSourceChangesOff() {
      String hocon = SourceChangesCapability.CONFIG_KEY + " = false";

      assertEquals(List.of(StylesheetChangesCapability.KEY, AiAssistantCapability.KEY),
          getAnnouncedFor(mock(App.class), hocon));
    }

    @Test
    @DisplayName("Should drop only the freeform changes when the freeform switch is off")
    void shouldDropOnlyFreeformWhenFreeformSwitchOff() {
      String hocon = SourceFreeformChangesCapability.CONFIG_KEY + " = false";

      assertEquals(List.of(SourceChangesCapability.KEY, StylesheetChangesCapability.KEY,
          AiAssistantCapability.KEY), getAnnouncedFor(mock(App.class), hocon));
    }

    @Test
    @DisplayName("Should drop the source and the freeform changes for a Kotlin application")
    void shouldDropSourceAndFreeformForKotlinApplication() {
      String hocon = """
          webforj.devtools.craftforj {
            source-changes = true
            stylesheet-changes = true
            ai.enabled = true
            ai.freeform-changes = true
          }
          """;

      assertEquals(List.of(StylesheetChangesCapability.KEY, AiAssistantCapability.KEY),
          getAnnouncedFor(new KotlinApp(), hocon));
    }
  }

  private static CapabilitiesProvider buildProvider(App app, boolean licensed,
      List<CraftforjCapability> capabilities) {
    return CapabilitiesProvider.create(app, licensed)
        .setVersionDetector(new VersionDetector("26.02"))
        .setFrameworkVersionDetector(new FrameworkVersionDetector("26.02"))
        .setCapabilities(capabilities).build();
  }

  private static List<String> getAnnouncedFor(App app, String hocon) {
    Environment environment = mock(Environment.class);
    when(environment.getConfig()).thenReturn(ConfigFactory.parseString(hocon));

    try (MockedStatic<Environment> mocked = mockStatic(Environment.class)) {
      mocked.when(Environment::getCurrent).thenReturn(environment);

      return new CapabilitiesProvider(app, true).getCapabilities();
    }
  }

  private static CraftforjCapability createFake(String key, boolean supported) {
    return new FakeCapability(key, app -> supported);
  }

  private static CraftforjCapability createRecordingFake(String key, List<App> seen,
      boolean supported) {
    return new FakeCapability(key, app -> {
      seen.add(app);
      return supported;
    });
  }

  private static void setOrClearProperty(String key, String value) {
    if (value == null) {
      System.clearProperty(key);
    } else {
      System.setProperty(key, value);
    }
  }

  private static final class FakeCapability implements CraftforjCapability {

    private final String key;
    private final Predicate<App> check;

    private FakeCapability(String key, Predicate<App> check) {
      this.key = key;
      this.check = check;
    }

    @Override
    public String getKey() {
      return key;
    }

    @Override
    public boolean isSupported(App app) {
      return check.test(app);
    }
  }

  @kotlin.Metadata
  private static final class KotlinApp extends App {
  }
}
