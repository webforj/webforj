package org.dwcj.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The NodeAttribute annotation is used to set attributes on the web component 
 * upon creation.
 * 
 * For example, the following annotation will set the "disabled" attribute to
 * "true" on the web component when it is created.
 * 
 * <pre>
 * {@code
 * &#64;NodeAttribute(name = "disabled", value = "true")
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(NodeAttribute.Container.class)
@Inherited
@Documented
public @interface NodeAttribute {
  /** The name of the attribute */
  String name();

  /** The value of the attribute */
  String value() default "";

  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    NodeAttribute[] value();
  }
}
