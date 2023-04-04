package org.dwcj.component.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The HtmlViewClassName annotation is used to set css classes on the html view hosting the web
 * component upon creation.
 *
 * For example, the following annotation will set the class "my-class" on the html view hosting the
 * web component when it is created.
 *
 * <pre>
 * {@code
 * &#64;HtmlViewClass("my-class")
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(HtmlViewClassName.Container.class)
@Inherited
@Documented
public @interface HtmlViewClassName {
  /**
   * An array of class names to be added to html view hosting the a node.
   **/
  String[] value() default {};

  /**
   * The container annotation for {@link HtmlViewClassName} annotation.
   *
   * @see HtmlViewClassName
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * An array of {@link HtmlViewClassName} annotations.
     *
     * @return an array of {@link HtmlViewClassName} annotations
     */
    HtmlViewClassName[] value();
  }
}
