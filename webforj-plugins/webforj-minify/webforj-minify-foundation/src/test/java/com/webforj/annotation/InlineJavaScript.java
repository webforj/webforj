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
@Repeatable(InlineJavaScript.Container.class)
public @interface InlineJavaScript {
  /**
   * The inline JavaScript code or file URL.
   *
   * @return the inline JavaScript code or file URL
   */
  String value();

  /**
   * Container annotation for repeatable InlineJavaScript annotations.
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @interface Container {
    /**
     * Array of InlineJavaScript annotations.
     *
     * @return array of InlineJavaScript annotations
     */
    InlineJavaScript[] value();
  }
}
