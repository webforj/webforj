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
@Repeatable(JavaScript.Container.class)
public @interface JavaScript {
  /**
   * The JavaScript file URL.
   *
   * @return the JavaScript file URL
   */
  String value();

  /**
   * Container annotation for repeatable JavaScript annotations.
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @interface Container {
    /**
     * Array of JavaScript annotations.
     *
     * @return array of JavaScript annotations
     */
    JavaScript[] value();
  }
}
