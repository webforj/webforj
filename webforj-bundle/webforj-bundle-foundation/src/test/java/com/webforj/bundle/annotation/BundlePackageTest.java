package com.webforj.bundle.annotation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.annotation.Annotation;
import org.junit.jupiter.api.Test;

class BundlePackageTest {

  @BundlePackage(value = "react", version = "^19.0.0")
  static class SinglePackageHolder {
  }

  @BundlePackage(value = "react", version = "^19.0.0")
  @BundlePackage(value = "lit", version = "^3.0.0")
  static class MultiPackageHolder {
  }

  @Test
  void shouldExposeSinglePackageAtRuntime() {
    BundlePackage[] declared = SinglePackageHolder.class.getAnnotationsByType(BundlePackage.class);
    assertEquals(1, declared.length);
    assertEquals("react", declared[0].value());
    assertEquals("^19.0.0", declared[0].version());
  }

  @Test
  void shouldCaptureMultiplePackages() {
    BundlePackage[] declared = MultiPackageHolder.class.getAnnotationsByType(BundlePackage.class);
    assertEquals(2, declared.length);
    assertEquals("react", declared[0].value());
    assertEquals("lit", declared[1].value());
  }

  @Test
  void shouldAttachContainerWhenRepeated() {
    BundlePackage.Container container =
        MultiPackageHolder.class.getAnnotation(BundlePackage.Container.class);
    assertNotNull(container);
    assertEquals(2, container.value().length);
  }

  @Test
  void shouldRetainAtRuntime() {
    Annotation[] annotations = SinglePackageHolder.class.getAnnotations();
    boolean hasBundlePackage = false;
    for (Annotation a : annotations) {
      if (a.annotationType() == BundlePackage.class) {
        hasBundlePackage = true;
      }
    }
    assertTrue(hasBundlePackage);
  }
}
