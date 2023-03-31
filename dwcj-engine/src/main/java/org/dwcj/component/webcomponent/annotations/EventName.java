package org.dwcj.component.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define a web component DOM event name.
 * 
 * For example, the following annotation will define the "click" event name for
 * the web component.
 * 
 * <pre>
 * {@code
 * @EventName("click")
 * }
 * </pre>
 * 
 * @see <a href=
 *      "https://developer.mozilla.org/en-US/docs/Web/Events">https://developer.mozilla.org/en-US/docs/Web/Events</a>
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface EventName {

  /**
   * The name of the dom event as defined in the web component.
   * 
   * @return the name of the dom event
   **/
  String value();
}
