package org.dwcj.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The NodeClass annotation is used to set css classes on the web component
 * upon creation.
 * 
 * For example, the following annotation will set the class "my-class" on the
 * web component when it is created.
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
@Repeatable(NodeClass.Container.class)
@Inherited
@Documented
public @interface NodeClass {
  /**
   * An array of class names to be added to node.
   **/
  String[] value() default {};

  /**
   * The container annotation for {@link NodeClass} annotation.
   * 
   * @see NodeClass
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * An array of {@link NodeClass} annotations.
     * 
     * @return an array of {@link NodeClass} annotations
     */
    NodeClass[] value();
  }
}
