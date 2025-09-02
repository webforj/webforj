package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to inject a Link into the web page.
 *
 * <p>
 * The annotation can be used on the class level only and the class must extend `com.webforj.App` in
 * order for the annotation to be processed.
 * </p>
 *
 * <pre>
 * {@code @Link(url = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap")}
 * {@code @Link(url = "https://fonts.googleapis.com/icon?family=Material+Icons", top = true)}
 * </pre>
 *
 * @see InlineStyleSheet
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(Link.Container.class)
@Inherited
@Documented
public @interface Link {

  /**
   * A unique resource id. Can be used to avoid duplications
   *
   * @return the id of the resource
   **/
  String id() default "";

  /**
   * A link URL to be injected into this web page as a link element..
   *
   * @return the link URL
   **/
  String value();

  /**
   * A boolean value specifying whether this link is to be injected into the head (true) or body
   * (false) section.
   *
   * @return true if the link is to be injected into the head section
   */
  boolean top() default true;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link#attributes">attributes</a> to
   * be added to the
   * <a href= "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link">link</a> element.
   * Attributes can be specified either as a string in the format "attr=value,attr=value" or as a
   * HashMap containing key/value pairs.
   *
   * @return the attributes of the link element
   */
  Attribute[] attributes() default {};

  /**
   * A container for multiple {@link Link} annotations.
   *
   * @see Link
   * @author Hyyan Abo Fakher
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {
    /**
     * The multiple {@link Link} annotations.
     *
     * @return the multiple {@link Link} annotations
     */
    Link[] value();
  }
}
