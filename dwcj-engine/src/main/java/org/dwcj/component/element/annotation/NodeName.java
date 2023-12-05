package org.dwcj.component.element.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define the tag name of an element.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface NodeName {

  /** The tag name of the element. */
  String value() default "";
}
