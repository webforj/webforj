package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to inject a CSS content into the web page.
 * The annotation can be used on the AppLevel or the control level.
 * 
 * <pre>
 * {@code
 * &#64;StyleSheet(url = "https://www.w3schools.com/w3css/4/w3.css")
 * &#64;StyleSheet(url = "https://www.w3schools.com/w3css/4/w3.css", top = true)
 * }
 * </pre>
 * 
 * @see InlineStyleSheet
 * @Author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(StyleSheet.Container.class)
@Inherited
@Documented
@java.lang.SuppressWarnings("squid:S2786")
public @interface StyleSheet {

  /** A unique resource id. Can be used to avoid duplications */
  String id() default "";

  /** A CSS URL to be injected into this web page as a style element. */
  String url();

  /**
   * A boolean value specifying whether this style is to be injected into the top
   * level window of the page.
   */
  boolean top() default false;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/style">attributes</a>
   * to be added to the style element.
   * Attributes can be specified either as a string in the format
   * "attr=value,attr=value" or as a HashMap containing key/value pairs.
   */
  Attribute[] attributes() default {};

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {
    /** A list of style sheets to inject in the app */
    StyleSheet[] value();
  }
}
