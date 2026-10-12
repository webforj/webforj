package com.webforj.bundle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

import com.webforj.Page;
import com.webforj.environment.ObjectTable;
import com.webforj.utilities.Assets;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BundleAssetLoaderTest {

  static class WithCounter {
  }

  static class WithCounterAndHello {
  }

  static class WithoutEntry {
  }

  static class SubclassWithoutOwnBinding extends WithCounter {
  }

  @AfterEach
  void resetIndexCache() {
    BundleIndexStore.reset();
  }

  @Test
  void shouldResolveSingleBindingToScriptUrl() {
    BundleAssetLoader loader = new BundleAssetLoader(index(WithCounter.class, "counter-A1B2.js"));

    List<String> urls = loader.resolveUrlsFor(WithCounter.class);

    assertEquals(List.of("ws://frontend/counter-A1B2.js"), urls);
  }

  @Test
  void shouldResolveBindingWithScriptAndStylesheet() {
    BundleAssetLoader loader =
        new BundleAssetLoader(index(WithCounter.class, "counter-A1B2.js", "counter-C3D4.css"));

    List<String> urls = loader.resolveUrlsFor(WithCounter.class);

    assertEquals(List.of("ws://frontend/counter-A1B2.js", "ws://frontend/counter-C3D4.css"), urls);
  }

  @Test
  void shouldResolveEveryOutputBoundToTheClassInOrder() {
    BundleAssetLoader loader =
        new BundleAssetLoader(index(WithCounterAndHello.class, "counter-A1B2.js", "hello-C3D4.js"));

    List<String> urls = loader.resolveUrlsFor(WithCounterAndHello.class);

    assertEquals(List.of("ws://frontend/counter-A1B2.js", "ws://frontend/hello-C3D4.js"), urls);
  }

  @Test
  void shouldSkipClassWithNoBinding() {
    BundleAssetLoader loader = new BundleAssetLoader(index(WithCounter.class, "counter-A1B2.js"));

    assertTrue(loader.resolveUrlsFor(WithoutEntry.class).isEmpty());
  }

  @Test
  void shouldResolveByExactClassNameWithoutClimbingSuperclasses() {
    // @Inherited is resolved at build time, the scan records the inherited binding on the subclass
    // itself, so the loader looks a class up by its own name and never walks superclasses.
    BundleAssetLoader loader = new BundleAssetLoader(index(WithCounter.class, "counter-A1B2.js"));

    assertTrue(loader.resolveUrlsFor(SubclassWithoutOwnBinding.class).isEmpty());
  }

  @Test
  void shouldResolveNothingWhenNoIndexIsAvailable() {
    BundleAssetLoader loader = new BundleAssetLoader();

    assertTrue(loader.resolveUrlsFor(WithCounter.class).isEmpty());
  }

  @Test
  void shouldSkipDebugOnlyFileWhenNotInDebugMode() {
    BundleAssetLoader loader = new BundleAssetLoader(indexWithDebug(WithCounter.class,
        List.of("counter-A1B2.js", "panel-D3.js"), List.of("panel-D3.js")));

    assertEquals(List.of("ws://frontend/counter-A1B2.js"),
        loader.resolveUrlsFor(WithCounter.class, false));
  }

  @Test
  void shouldInjectDebugOnlyFileWhenInDebugMode() {
    BundleAssetLoader loader = new BundleAssetLoader(indexWithDebug(WithCounter.class,
        List.of("counter-A1B2.js", "panel-D3.js"), List.of("panel-D3.js")));

    assertEquals(List.of("ws://frontend/counter-A1B2.js", "ws://frontend/panel-D3.js"),
        loader.resolveUrlsFor(WithCounter.class, true));
  }

  @Test
  void shouldResolveNothingWhenOnlyBindingIsDebugOnlyOutsideDebugMode() {
    BundleAssetLoader loader = new BundleAssetLoader(
        indexWithDebug(WithCounter.class, List.of("panel-D3.js"), List.of("panel-D3.js")));

    assertTrue(loader.resolveUrlsFor(WithCounter.class, false).isEmpty());
  }

  @Test
  void shouldMarkAnInlinedStylesheetWithItsBundlePath() {
    Page page = mock(Page.class);
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put(BundleIndex.EAGER_KEY, List.of("bundle.css"));
    BundleAssetLoader loader = new BundleAssetLoader(new BundleIndex(bindings));

    try (MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class);
        MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(Assets::isPackagedOnBBj).thenReturn(false);
      pages.when(() -> Page.ifPresent(any())).thenAnswer(invocation -> {
        invocation.<Consumer<Page>>getArgument(0).accept(page);
        return null;
      });

      loader.loadEager();
    }

    verify(page).addInlineStyleSheet("context://static/frontend/bundle.css", false,
        Map.of("data-webforj-bundle", "bundle.css"));
  }

  @Test
  void shouldLinkTheEagerBundleWhenAssetsArePackagedOnBbj() {
    Page page = mock(Page.class);
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put(BundleIndex.EAGER_KEY, List.of("bundle.css", "bundle.js"));
    BundleAssetLoader loader = new BundleAssetLoader(new BundleIndex(bindings));

    try (MockedStatic<Page> pages = mockStatic(Page.class);
        MockedStatic<ObjectTable> table = mockStatic(ObjectTable.class);
        MockedStatic<Assets> assets = mockStatic(Assets.class)) {
      assets.when(Assets::isPackagedOnBBj).thenReturn(true);
      pages.when(() -> Page.ifPresent(any())).thenAnswer(invocation -> {
        invocation.<Consumer<Page>>getArgument(0).accept(page);
        return null;
      });

      loader.loadEager();
    }

    verify(page).addStyleSheet("ws://frontend/bundle.css", false, Map.of());
    verify(page).addJavaScript("ws://frontend/bundle.js", false, Map.of("type", "module"));
  }

  private static BundleIndex index(Class<?> boundClass, String... files) {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put(boundClass.getName(), List.of(files));

    return new BundleIndex(bindings);
  }

  private static BundleIndex indexWithDebug(Class<?> boundClass, List<String> files,
      List<String> debugFiles) {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put(boundClass.getName(), files);
    bindings.put(BundleIndex.DEBUG_KEY, debugFiles);

    return new BundleIndex(bindings);
  }
}
