package com.webforj.devtools.craftforj.utilities;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("KotlinClassDetector")
class KotlinClassDetectorTest {

  @kotlin.Metadata
  static class KotlinCompiled {
  }

  static class JavaCompiled {
  }

  @Nested
  @DisplayName("isKotlin(Class)")
  class IsKotlinByClass {

    @Test
    @DisplayName("detects the Kotlin metadata marker")
    void shouldDetectKotlinClass() {
      assertTrue(KotlinClassDetector.isKotlin(KotlinCompiled.class));
    }

    @Test
    @DisplayName("treats an unmarked class as Java")
    void shouldTreatUnmarkedClassAsJava() {
      assertFalse(KotlinClassDetector.isKotlin(JavaCompiled.class));
    }

    @Test
    @DisplayName("treats null as Java")
    void shouldTreatNullAsJava() {
      assertFalse(KotlinClassDetector.isKotlin((Class<?>) null));
    }
  }

  @Nested
  @DisplayName("isKotlin(String, ClassLoader)")
  class IsKotlinByName {

    @Test
    @DisplayName("resolves the name and detects the marker")
    void shouldResolveKotlinClassByName() {
      assertTrue(KotlinClassDetector.isKotlin(KotlinCompiled.class.getName(),
          KotlinCompiled.class.getClassLoader()));
    }

    @Test
    @DisplayName("never looks past the given loader")
    void shouldNotLookPastGivenLoader() {
      ClassLoader blind = new ClassLoader(null) {};

      assertFalse(KotlinClassDetector.isKotlin(KotlinCompiled.class.getName(), blind));
      assertFalse(KotlinClassDetector.isKotlin(KotlinCompiled.class.getName(), null));
    }

    @Test
    @DisplayName("treats an unknown or empty name as Java")
    void shouldTreatUnknownNameAsJava() {
      ClassLoader loader = KotlinCompiled.class.getClassLoader();

      assertFalse(KotlinClassDetector.isKotlin("com.example.Missing", loader));
      assertFalse(KotlinClassDetector.isKotlin("", loader));
      assertFalse(KotlinClassDetector.isKotlin((String) null, loader));
    }
  }
}
