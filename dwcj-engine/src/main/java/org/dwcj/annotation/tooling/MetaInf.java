package org.dwcj.annotation.tooling;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set information used by tools
 *
 * <p>
 * The annotation can be used on the class level or on the level of methods.
 * </p>
 *
 * @author Stephan Wald
 */

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface MetaInf {

  /**
   * determines the logical group of the component or class, to visually suppor the developer in
   * tools by grouping items.
   */
  String group() default "";

  /**
   * determines if the annotated class or method shall be hidden in tools etc.
   */
  boolean hidden() default false;

}
