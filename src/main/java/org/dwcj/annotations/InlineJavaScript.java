package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to inject a JavaScript content into the web page.
 * The annotation can be used on the AppLevel or the control level.
 * 
 * <pre>
 * {@code
 * &#64;InlineJavaScript(value = "alert('Hello World!');")
 * &#64;InlineJavaScript(value = "alert('Hello World!');", top = true)
 * &#64;InlineJavaScript(value = "js/script.js", local = true)
 * }
 * </pre>
 * 
 * @see JavaScript
 * @Author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(InlineJavaScript.Container.class)
@Inherited
@Documented
public @interface InlineJavaScript {
  
  /** 
   * A unique resource id. 
   * Once the id is set, the resource will be injected only once in the page.
   **/
  String id() default "";

  /**
   * A JavaScript content to be injected into this web page as a script element.
   */
  String value();

  /**
   * A boolean value specifying whether this script is to be injected into the top
   * level window of the page.
   */
  boolean top() default false;

  /**
   * A boolean value specifying whether this style is to be injected is in local
   * file.
   */
  boolean local() default false;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script">attributes</a>
   * to be added to the script element.
   * Attributes can be specified either as a string in the format
   * "attr=value,attr=value" or as a HashMap containing key/value pairs.
   */
  Attribute[] attributes() default {};

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {

    /** A list of inline JavaScripts to inject in the app */
    InlineJavaScript[] value();
  }
}
