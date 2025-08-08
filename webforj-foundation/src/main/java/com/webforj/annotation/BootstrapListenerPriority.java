package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the priority for a {@link com.webforj.BootstrapListener} implementation.
 *
 * <p>
 * This annotation controls the order in which bootstrap listeners are invoked during the bootstrap
 * lifecycle. Listeners with lower priority values are invoked before listeners with higher priority
 * values. If this annotation is not present, a default priority of 10 is used.
 * </p>
 *
 * <p>
 * The priority system allows for predictable ordering of bootstrap customizations, ensuring that
 * framework-level listeners can run before application-level listeners, and that dependencies
 * between listeners can be properly managed.
 * </p>
 *
 * <p>
 * Recommended priority ranges:
 * </p>
 * <ul>
 * <li>1-9: Framework-level listeners (core integrations, fundamental services)</li>
 * <li>10-50: Application-level listeners (normal application customizations)</li>
 * <li>51-100: Auxiliary listeners (logging, metrics, debugging)</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 *
 * @see com.webforj.BootstrapListener
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface BootstrapListenerPriority {

  /**
   * The priority value for this listener.
   *
   * <p>
   * Lower values indicate higher priority and will be executed first. The default value is 10 if
   * this annotation is not present on the listener class.
   * </p>
   *
   * <p>
   * Valid range is 1 to 100, though any integer value is technically allowed. It is recommended to
   * use the standard ranges documented in the class-level documentation for consistency across the
   * application.
   * </p>
   *
   * @return the priority value
   */
  int value() default 10;
}
