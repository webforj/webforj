package com.webforj.bundle.annotation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class BundleEntryTest {

  @BundleEntry("counter")
  static class SingleBundleHolder {
  }

  @BundleEntry("counter")
  @BundleEntry("hello")
  static class MultiBundleHolder {
  }

  @BundleEntry(value = "panel", debug = true)
  static class DebugBundleHolder {
  }

  @Test
  void shouldExposeSingleBundleAtRuntime() {
    BundleEntry[] bundles = SingleBundleHolder.class.getAnnotationsByType(BundleEntry.class);

    assertEquals(1, bundles.length);
    assertEquals("counter", bundles[0].value());
  }

  @Test
  void shouldCaptureMultipleBundles() {
    BundleEntry[] bundles = MultiBundleHolder.class.getAnnotationsByType(BundleEntry.class);

    assertEquals(2, bundles.length);
    assertEquals("counter", bundles[0].value());
    assertEquals("hello", bundles[1].value());
  }

  @Test
  void shouldDefaultDebugToFalse() {
    BundleEntry[] bundles = SingleBundleHolder.class.getAnnotationsByType(BundleEntry.class);

    assertFalse(bundles[0].debug());
  }

  @Test
  void shouldCaptureDebugFlagWhenSet() {
    BundleEntry[] bundles = DebugBundleHolder.class.getAnnotationsByType(BundleEntry.class);

    assertEquals("panel", bundles[0].value());
    assertTrue(bundles[0].debug());
  }
}
