package org.dwcj.component.webcomponent.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The HtmlViewAttribute annotation is used to set attributes on the html view hosting the web
 * component upon creation.
 *
 * <p>
 * For example, the following annotation will set the attribute "disabled" with value "true" on the
 * html view hosting the web component when it is created.
 * </p>
 *
 * <pre>
 * {@code
 * &#64;HtmlViewAttribute(name = "disabled", value = "true")
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(HtmlViewAttribute.Container.class)
@Inherited
@Documented
public @interface HtmlViewAttribute {
  /**
   * The name of the attribute.
   *
   * @return the name of the attribute
   **/
  String name();

  /**
   * The value of the attribute.
   *
   * @return the value of the attribute
   **/
  String value() default "";

  /**
   * The container annotation for {@link HtmlViewAttribute} annotation.
   *
   * @see HtmlViewAttribute
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * An array of {@link HtmlViewAttribute} annotations.
     *
     * @return an array of {@link HtmlViewAttribute} annotations
     */
    HtmlViewAttribute[] value();
  }
}
