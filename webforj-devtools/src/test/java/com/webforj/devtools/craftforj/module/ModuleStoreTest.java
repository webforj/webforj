package com.webforj.devtools.craftforj.module;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;

import com.webforj.Environment;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.module.model.ModuleSource;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class ModuleStoreTest {

  private static final String UI = "META-INF/resources/webforj/craftforj-ui.min.js";
  private static final String MISSING = "META-INF/resources/webforj/missing.js";

  private MockedStatic<Environment> environment;

  // Assets resolves the class loader through the running environment, which no unit test has.
  @BeforeEach
  void setUp() {
    environment = mockStatic(Environment.class);
    environment.when(Environment::getCurrent).thenReturn(mock(Environment.class));
  }

  @AfterEach
  void tearDown() {
    environment.close();
  }

  @Test
  @DisplayName("Should read a module as base64 paired with the digest of that payload")
  void shouldReadModuleWithDigest() {
    ModuleSource module = new ModuleStore(Map.of("ui", UI)).read("ui");

    assertTrue(module.getBase64().length() > 0);
    assertEquals(64, module.getSha256().length());
  }

  @Test
  @DisplayName("Should read the same digest for the same module twice")
  void shouldReadStableDigest() {
    ModuleStore store = new ModuleStore(Map.of("ui", UI));

    assertEquals(store.read("ui").getSha256(), store.read("ui").getSha256());
  }

  @Test
  @DisplayName("Should throw when the requested module is not in the catalog")
  void shouldThrowWhenModuleUnknown() {
    ModuleStore store = new ModuleStore(Map.of("ui", UI));

    CraftforjActionException ex =
        assertThrows(CraftforjActionException.class, () -> store.read("nonsense"));
    assertTrue(ex.getMessage().contains("Unknown craftforJ module"));
  }

  @Test
  @DisplayName("Should build a manifest naming every readable module with its digest")
  void shouldBuildManifest() {
    ModuleStore store = new ModuleStore(Map.of("ui", UI));

    assertEquals("ui:" + store.read("ui").getSha256(), store.getManifest());
  }

  @Test
  @DisplayName("Should separate manifest entries so each module is named once")
  void shouldSeparateManifestEntries() {
    Map<String, String> catalog = new LinkedHashMap<>();
    catalog.put("ui", UI);
    catalog.put("panel", UI);
    ModuleStore store = new ModuleStore(catalog);

    String digest = store.read("ui").getSha256();
    assertEquals("ui:" + digest + ",panel:" + digest, store.getManifest());
  }

  @Test
  @DisplayName("Should build a manifest carrying no quote of its own")
  void shouldBuildManifestWithoutQuotes() {
    String manifest = new ModuleStore(Map.of("ui", UI)).getManifest();

    assertFalse(manifest.contains("\""));
    assertFalse(manifest.contains("'"));
  }

  @Test
  @DisplayName("Should leave a module the build does not carry out of the manifest")
  void shouldSkipUnreadableModuleInManifest() {
    Map<String, String> catalog = new LinkedHashMap<>();
    catalog.put("ui", UI);
    catalog.put("gesture", MISSING);
    ModuleStore store = new ModuleStore(catalog);

    assertEquals("ui:" + store.read("ui").getSha256(), store.getManifest());
  }

  @Test
  @DisplayName("Should build an empty manifest when no module can be read")
  void shouldBuildEmptyManifest() {
    assertEquals("", new ModuleStore(Map.of("gesture", MISSING)).getManifest());
  }
}
