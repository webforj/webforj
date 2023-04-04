package org.dwcj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set an attribute on the document element of the web page.
 *
 * The annotation can be used on the class level only and the class must extend `org.dwcj.App` in
 * order for the annotation to be processed.
 *
 * <pre>
 * {@code
 * &#64;AppAttribute(name = "first-attr", value = "value1")
 * @AppAttribute(name = "second-attr", value = "value2", selector = "body")
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(AppAttribute.Container.class)
@Inherited
@Documented
public @interface AppAttribute {
  /**
   * The name of the attribute
   *
   * @return the name of the attribute
   **/
  String name();

  /**
   * The value of the attribute
   *
   * @return the value of the attribute
   **/
  String value();

  /**
   * By default, setAttribute applies to the <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement">document</a>
   * element on the web page. If a selector is specified, it selects a descendant element within the
   * document to set this attribute. If a specified
   * <a href= "https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector">selector</a>
   * doesn't return any elements, the default document element is used.
   *
   * @return the selector of the element to set the attribute on
   */
  String selector() default "";

  /**
   * A container for multiple {@link AppAttribute} annotations.
   *
   * @see AppAttribute
   * @author Hyyan Abo Fakher
   */
  @Target(ElementType.TYPE)
  @Retention(RetentionPolicy.RUNTIME)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * The array of {@link AppAttribute} annotations.
     *
     * @return the array of {@link AppAttribute} annotations
     */
    AppAttribute[] value();
  }
}
