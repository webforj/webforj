package org.dwcj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to inject a CSS content into the web page. The annotation can be used on the
 * AppLevel or the control level.
 *
 * <pre>
 * {@code
 * &#64;InlineStyleSheet(value = "body {background-color: red;}")
 * &#64;InlineStyleSheet(value = "body {background-color: red;}", top = true)
 * @InlineStyleSheet(value = "css/style.css", local = true)
 * }
 * </pre>
 *
 * @see StyleSheet
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(InlineStyleSheet.Container.class)
@Inherited
@Documented
public @interface InlineStyleSheet {

  /**
   * A unique resource id.
   *
   * Giving the resources the same id will cause them to be bundled together. Bundling here means
   * that the resources will be injected in the same style element in the page.
   *
   * If you want to inject the resource only once then use the {@link #once()} attribute.
   *
   * @return the unique resource id
   **/
  String id() default "";

  /**
   * A CSS content to be injected into this web page as a style element.
   *
   * @return the CSS content
   **/
  String value();

  /**
   * A boolean value specifying whether this style is to be injected into the top level window of
   * the page.
   *
   * @return true if the style is to be injected into the top level window
   */
  boolean top() default false;

  /**
   * A boolean value specifying whether this style is to be injected into the current page only
   * once.
   *
   * This can be useful when creating a custom component that comes with its own style sheet.
   * Multiple instances of the component can be used but the style sheet should be injected only
   * once.
   *
   * @return true if the style is to be injected into the current page only once
   */
  boolean once() default true;

  /**
   * A set of
   * <a href= "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style">attributes</a> to be
   * added to the style element. Attributes can be specified either as a string in the format
   * "attr=value,attr=value" or as a HashMap containing key/value pairs.
   *
   * @return the attributes
   */
  Attribute[] attributes() default {};

  /**
   * A container for multiple {@link InlineStyleSheet} annotations.
   *
   * @see InlineStyleSheet
   * @author Hyyan Abo Fakher
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {

    /** A list of inline style sheets to inject in the app */
    InlineStyleSheet[] value();
  }
}
