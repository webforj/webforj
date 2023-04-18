package org.dwcj.component.webcomponent.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to define the tag name of the web component node.
 *
 * <p>
 * This annotation is processed by the {@link org.dwcj.component.webcomponent.WebComponent} class to
 * define the tag name of the web component node.
 *
 * For example, the following annotation will define the "my-component" tag name for the web
 * component.
 * </p>
 *
 * <pre>
 * {@code
 * &#64;NodeName("my-component")
 * }
 * </pre>
 *
 * If the value of this annotation is not defined, then the HTMLContainer will will be used as the
 * root.
 *
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface NodeName {

  /** The tag name of the web component. */
  String value() default "";
}
