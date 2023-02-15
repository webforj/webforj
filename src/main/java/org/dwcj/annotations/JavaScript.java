package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to inject a JavaScript URL into the web page.
 * The annotation can be used on the AppLevel or the control level.
 * 
 * <pre>
 * {@code
 * &#64;JavaScript(url = "http://www.example.com/script.js")
 * &#64;JavaScript(url = "http://www.example.com/script.js", top = true)
 * }
 * </pre>
 * 
 * @see InlineJavaScript
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(JavaScript.Container.class)
@Inherited
@Documented
public @interface JavaScript {

  /** A JavaScript URL to be injected into this web page as a script element. */
  String url();

  /**
   * A boolean value specifying whether this script is to be injected into the top
   * level window of the page.
   */
  boolean top() default false;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script">attributes</a>
   * to be added to the script element.
   */
  Attribute[] attributes() default {};

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {

    /** A list of JavaScripts to inject in the app */
    JavaScript[] value();
  }

}
