package com.webforj.data.binding.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to bind a component to the given bean property name.
 *
 * @author Hyyan Abo Fakher
 * @since 24.01
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface UseProperty {

  /**
   * The name of the bean property to bind to.
   *
   * @return the name of the bean property
   */
  String value();
}
