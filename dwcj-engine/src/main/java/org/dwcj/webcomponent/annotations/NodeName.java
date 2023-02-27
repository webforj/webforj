package org.dwcj.webcomponent.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define the tag name of the web component node.
 * 
 * This annotation is processed by the
 * {@link org.dwcj.webcomponent.WebComponent}
 * class to define the tag name of the web component node.
 * 
 * For example, the following annotation will define the "my-component" tag name
 * for the web component.
 * 
 * <pre>
 * {@code
 * @NodeName("my-component")
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface NodeName {

  /** The tag name of the web component */
  String value();
}
