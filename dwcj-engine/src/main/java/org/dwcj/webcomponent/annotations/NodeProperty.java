package org.dwcj.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The NodeProperty annotation is used to set properties on the web component
 * upon creation.
 * 
 * For example, the following annotation will set the "disabled" property to
 * "true" on the web component when it is created.
 * 
 * <pre>
 * {@code
 * @NodeProperty(name = "disabled", value = "true")
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(NodeProperty.Container.class)
@Inherited
@Documented
public @interface NodeProperty {
  /**
   * The name of the property to set on the web component.
   * 
   * @return the name of the property
   **/
  String name();

  /**
   * The value of the property to set on the web component.
   * 
   * @return the value of the property
   **/
  String value() default "";

  /**
   * A container for the {@link NodeProperty} annotation.
   * 
   * @see NodeProperty
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * The array of {@link NodeProperty} annotations.
     * 
     * @return the array of {@link NodeProperty} annotations
     */
    NodeProperty[] value();
  }
}
