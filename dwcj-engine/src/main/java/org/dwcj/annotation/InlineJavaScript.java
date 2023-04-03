package org.dwcj.annotation;

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
 * @InlineJavaScript(value = "alert('Hello World!');")
 * @InlineJavaScript(value = "alert('Hello World!');", top = true)
 * @InlineJavaScript(value = "js/script.js", local = true)
 * }
 * </pre>
 * 
 * @see JavaScript
 * @author Hyyan Abo Fakher
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
   * 
   * @return the unique resource id
   **/
  String id() default "";

  /**
   * A JavaScript content to be injected into this web page as a script element.
   * 
   * @return the JavaScript content
   */
  String value();

  /**
   * A boolean value specifying whether this script is to be injected into the top
   * level window of the page.
   * 
   * @return true if the script is to be injected into the top level window of the
   */
  boolean top() default false;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script">attributes</a>
   * to be added to the script element.
   * Attributes can be specified either as a string in the format
   * "attr=value,attr=value" or as a HashMap containing key/value pairs.
   * 
   * @return the attributes to be added to the script element
   */
  Attribute[] attributes() default {};

  /**
   * A container for multiple {@link InlineJavaScript} annotations.
   * 
   * @see InlineJavaScript
   * @author Hyyan Abo Fakher
   */
  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {

    /**
     * The multiple {@link InlineJavaScript} annotations.
     * 
     * @return the multiple {@link InlineJavaScript} annotations
     */
    InlineJavaScript[] value();
  }
}
