package com.webforj.component.element.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define an element DOM event name.
 *
 * <p>
 * For example, the following annotation will define the "click" event name for the element.
 * </p>
 *
 * <pre>
 * {@code
 * &#64;EventName("click")
 * }
 * </pre>
 *
 * @see <a href=
 *      "https://developer.mozilla.org/en-US/docs/Web/Events">https://developer.mozilla.org/en-US/docs/Web/Events</a>
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface EventName {

  /**
   * The name of the DOM event.
   *
   * @return the name of the DOM event
   **/
  String value();
}
