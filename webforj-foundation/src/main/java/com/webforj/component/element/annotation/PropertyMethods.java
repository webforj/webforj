package com.webforj.component.element.annotation;

import com.webforj.component.element.PropertyDescriptorTester;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to hint the {@link PropertyDescriptorTester} about the property getter and setter.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface PropertyMethods {
  /**
   * The name of the getter method for the property.
   *
   * @return The name of the getter method for the property.
   */
  String getter() default "";

  /**
   * The name of the setter method for the property.
   *
   * @return The name of the setter method for the property.
   */
  String setter() default "";

  /**
   * The target class of the getter and setter methods.
   *
   * @return The target class of the getter and setter methods.
   */
  Class<?> target() default void.class;
}
