package com.webforj.minify.processor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import org.junit.jupiter.api.Test;

/**
 * Unit tests for AssetAnnotationProcessor.ResourceEntry.
 *
 * <p>These tests verify the equals() and hashCode() contract to increase branch coverage.
 *
 * @author Kevin Hagel
 */
class ResourceEntryTest {

  @Test
  void testEqualsSameObject() throws Exception {
    Object entry = createResourceEntry("test.css", "StyleSheet", "com.example.App");

    assertTrue(invokeEquals(entry, entry), "An object should equal itself");
  }

  @Test
  void testEqualsNull() throws Exception {
    Object entry = createResourceEntry("test.css", "StyleSheet", "com.example.App");

    assertFalse(invokeEquals(entry, null), "Object should not equal null");
  }

  @Test
  void testEqualsDifferentClass() throws Exception {
    Object entry = createResourceEntry("test.css", "StyleSheet", "com.example.App");
    String differentType = "not a ResourceEntry";

    assertFalse(invokeEquals(entry, differentType),
        "Object should not equal instance of different class");
  }

  @Test
  void testEqualsEqualObjects() throws Exception {
    Object entry1 = createResourceEntry("test.css", "StyleSheet", "com.example.App");
    Object entry2 = createResourceEntry("test.css", "StyleSheet", "com.example.DifferentApp");

    assertTrue(invokeEquals(entry1, entry2),
        "Objects with same url and type should be equal (discoveredIn is not compared)");
  }

  @Test
  void testEqualsDifferentUrl() throws Exception {
    Object entry1 = createResourceEntry("test1.css", "StyleSheet", "com.example.App");
    Object entry2 = createResourceEntry("test2.css", "StyleSheet", "com.example.App");

    assertFalse(invokeEquals(entry1, entry2), "Objects with different URLs should not be equal");
  }

  @Test
  void testEqualsDifferentType() throws Exception {
    Object entry1 = createResourceEntry("test.css", "StyleSheet", "com.example.App");
    Object entry2 = createResourceEntry("test.css", "JavaScript", "com.example.App");

    assertFalse(invokeEquals(entry1, entry2), "Objects with different types should not be equal");
  }

  @Test
  void testHashCodeConsistency() throws Exception {
    Object entry = createResourceEntry("test.css", "StyleSheet", "com.example.App");

    int hash1 = entry.hashCode();
    int hash2 = entry.hashCode();

    assertEquals(hash1, hash2, "hashCode should return same value on multiple calls");
  }

  @Test
  void testHashCodeEqualObjects() throws Exception {
    Object entry1 = createResourceEntry("test.css", "StyleSheet", "com.example.App");
    Object entry2 = createResourceEntry("test.css", "StyleSheet", "com.example.DifferentApp");

    assertEquals(entry1.hashCode(), entry2.hashCode(),
        "Equal objects should have equal hash codes");
  }

  @Test
  void testHashCodeDifferentObjects() throws Exception {
    Object entry1 = createResourceEntry("test1.css", "StyleSheet", "com.example.App");
    Object entry2 = createResourceEntry("test2.css", "StyleSheet", "com.example.App");

    assertNotEquals(entry1.hashCode(), entry2.hashCode(),
        "Different objects should (usually) have different hash codes");
  }

  /**
   * Helper method to create ResourceEntry instance using reflection.
   *
   * <p>ResourceEntry is a private inner class of AssetAnnotationProcessor, so we need reflection
   * to instantiate it for testing.
   */
  private Object createResourceEntry(String url, String type, String discoveredIn)
      throws Exception {
    Class<?> processorClass = AssetAnnotationProcessor.class;
    Class<?>[] innerClasses = processorClass.getDeclaredClasses();

    Class<?> resourceEntryClass = null;
    Class<?> expectedClass = Class.forName(
        "com.webforj.minify.processor.AssetAnnotationProcessor$ResourceEntry");
    for (Class<?> innerClass : innerClasses) {
      if (expectedClass.isAssignableFrom(innerClass)) {
        resourceEntryClass = innerClass;
        break;
      }
    }

    if (resourceEntryClass == null) {
      throw new IllegalStateException("ResourceEntry class not found");
    }

    Constructor<?> constructor =
        resourceEntryClass.getDeclaredConstructor(String.class, String.class, String.class);
    constructor.setAccessible(true);
    return constructor.newInstance(url, type, discoveredIn);
  }

  /**
   * Helper method to invoke equals() method using reflection.
   */
  private boolean invokeEquals(Object obj1, Object obj2) throws Exception {
    return (Boolean) obj1.getClass().getMethod("equals", Object.class).invoke(obj1, obj2);
  }
}
