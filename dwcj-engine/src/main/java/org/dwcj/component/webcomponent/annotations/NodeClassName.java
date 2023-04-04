package org.dwcj.component.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The NodeClassName annotation is used to set css classes on the web component upon creation.
 *
 * <p>
 * For example, the following annotation will set the class "my-class" on the web component when it
 * is created.
 * </p>
 *
 * <pre>
 * {@code
 * &#64;NodeClass("my-class")
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(NodeClassName.Container.class)
@Inherited
@Documented
public @interface NodeClassName {
  /**
   * An array of class names to be added to node.
   **/
  String[] value() default {};

  /**
   * The container annotation for {@link NodeClassName} annotation.
   *
   * @see NodeClassName
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * An array of {@link NodeClassName} annotations.
     *
     * @return an array of {@link NodeClassName} annotations
     */
    NodeClassName[] value();
  }
}
