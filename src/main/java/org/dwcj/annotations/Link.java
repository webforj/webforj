package org.dwcj.annotations;

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
 * The annotation can be used on the class level only and the class must extend
 * `org.dwcj.App` in order for the annotation to be processed.
 * 
 * <pre>
 * {@code
 * &#64;Link(url = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap")
 * &#64;Link(url = "https://fonts.googleapis.com/icon?family=Material+Icons", top = true)
 * }
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

  /** A unique resource id. Can be used to avoid duplications */
  String id() default "";

  /** A link URL to be injected into this web page as a link element.. */
  String url();

  /**
   * A boolean value specifying whether this link is to be injected into the head
   * (true) or body (false) section.
   */
  boolean top() default true;

  /**
   * A set of <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link#attributes">attributes</a>
   * to be added to the <a href=
   * "https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link">link</a>
   * element. Attributes can be
   * specified either as a string in the format "attr=value,attr=value" or as a
   * HashMap containing key/value pairs.
   */
  Attribute[] attributes() default {};

  @Retention(RetentionPolicy.RUNTIME)
  @Target(ElementType.TYPE)
  @Inherited
  @Documented
  public @interface Container {
    /** A list of style sheets to inject in the app */
    Link[] value();
  }
}
