package com.webforj.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Stub annotation for testing the AssetAnnotationProcessor.
 *
 * @author Kevin Hagel
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(StyleSheet.Container.class)
public @interface StyleSheet {
  /**
   * The CSS file URL.
   *
   * @return the CSS file URL
   */
  String value();

  /**
   * Container annotation for repeatable StyleSheet annotations.
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @interface Container {
    /**
     * Array of StyleSheet annotations.
     *
     * @return array of StyleSheet annotations
     */
    StyleSheet[] value();
  }
}
