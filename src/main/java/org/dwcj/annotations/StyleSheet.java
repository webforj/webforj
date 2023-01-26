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
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(StyleSheet.Container.class)
@Inherited
@Documented
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
