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
@Repeatable(InlineStyleSheet.Container.class)
public @interface InlineStyleSheet {
  /**
   * The inline CSS code or file URL.
   *
   * @return the inline CSS code or file URL
   */
  String value();

  /**
   * Container annotation for repeatable InlineStyleSheet annotations.
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @interface Container {
    /**
     * Array of InlineStyleSheet annotations.
     *
     * @return array of InlineStyleSheet annotations
     */
    InlineStyleSheet[] value();
  }
}
