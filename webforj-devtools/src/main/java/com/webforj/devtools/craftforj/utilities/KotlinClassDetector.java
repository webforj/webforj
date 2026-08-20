package com.webforj.devtools.craftforj.utilities;

import java.lang.annotation.Annotation;

/**
 * Tells Kotlin compiled classes apart from Java ones.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class KotlinClassDetector {

  /**
   * The annotation the Kotlin compiler puts on every class it emits.
   */
  public static final String KOTLIN_METADATA = "kotlin.Metadata";

  private KotlinClassDetector() {
    // no-op
  }

  /**
   * Checks whether a class was compiled from Kotlin.
   *
   * @param type the class to inspect, may be {@code null}
   * @return {@code true} when the class carries the Kotlin metadata annotation
   */
  public static boolean isKotlin(Class<?> type) {
    if (type == null) {
      return false;
    }

    for (Annotation annotation : type.getAnnotations()) {
      if (KOTLIN_METADATA.equals(annotation.annotationType().getName())) {
        return true;
      }
    }

    return false;
  }

  /**
   * Checks whether a class named by its fully qualified name was compiled from Kotlin.
   *
   * @param className the fully qualified class name, may be {@code null}
   * @param loader the class loader that owns the class, may be {@code null}
   * @return {@code true} when the class resolves in that loader and carries the Kotlin metadata
   *         annotation
   */
  public static boolean isKotlin(String className, ClassLoader loader) {
    if (className == null || className.isEmpty() || loader == null) {
      return false;
    }

    try {
      return isKotlin(Class.forName(className, false, loader));
    } catch (ClassNotFoundException | LinkageError e) {

      return false;
    }
  }
}
