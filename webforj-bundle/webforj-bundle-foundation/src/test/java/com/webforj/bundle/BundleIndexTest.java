package com.webforj.bundle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class BundleIndexTest {

  @Test
  void shouldExposeBindings() {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CounterView", List.of("counter-A1B2.js", "counter-C3D4.css"));
    bindings.put("HelloView", List.of("hello-E5F6.js"));

    BundleIndex index = new BundleIndex(bindings);

    assertEquals(List.of("counter-A1B2.js", "counter-C3D4.css"),
        index.getBindings().get("CounterView"));
    assertEquals(List.of("hello-E5F6.js"), index.getBindings().get("HelloView"));
  }

  @Test
  void shouldSplitTheEagerBundleOutOfBindings() {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put(BundleIndex.EAGER_KEY, List.of("generated/bundle-A1B2.js"));

    BundleIndex index = new BundleIndex(bindings);

    assertEquals(List.of("generated/bundle-A1B2.js"), index.getEagerFiles());
    assertTrue(index.getBindings().isEmpty());
  }

  @Test
  void shouldSplitDebugFilesOutOfBindingsButKeepThemOnTheirClass() {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CounterView", List.of("counter-A1B2.js", "panel-D3.js"));
    bindings.put(BundleIndex.DEBUG_KEY, List.of("panel-D3.js"));

    BundleIndex index = new BundleIndex(bindings);

    assertEquals(List.of("panel-D3.js"), index.getDebugFiles());
    assertEquals(List.of("counter-A1B2.js", "panel-D3.js"), index.getBindings().get("CounterView"));
    assertNull(index.getBindings().get(BundleIndex.DEBUG_KEY));
  }

  @Test
  void shouldSplitGlobalFilesOutOfBindings() {
    Map<String, List<String>> bindings = new LinkedHashMap<>();
    bindings.put("CounterView", List.of("counter-A1B2.js"));
    bindings.put(BundleIndex.GLOBAL_KEY, List.of("generated/theme/theme-G7.css"));

    BundleIndex index = new BundleIndex(bindings);

    assertEquals(List.of("generated/theme/theme-G7.css"), index.getGlobalFiles());
    assertEquals(List.of("counter-A1B2.js"), index.getBindings().get("CounterView"));
    assertNull(index.getBindings().get(BundleIndex.GLOBAL_KEY));
  }

  @Test
  void shouldDefaultEagerAndDebugAndGlobalFilesToEmpty() {
    BundleIndex index = new BundleIndex(Map.of("CounterView", List.of("counter-A1B2.js")));

    assertTrue(index.getEagerFiles().isEmpty());
    assertTrue(index.getDebugFiles().isEmpty());
    assertTrue(index.getGlobalFiles().isEmpty());
  }

  @Test
  void shouldExposeImmutableBindingsMap() {
    BundleIndex index = new BundleIndex(Map.of("CounterView", List.of("counter-A1B2.js")));
    Map<String, List<String>> bindings = index.getBindings();
    List<String> value = List.of("oops.js");

    assertThrows(UnsupportedOperationException.class, () -> bindings.put("oops", value));
  }
}
