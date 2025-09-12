package com.webforj.router.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the priority for a RouteRendererObserver. Lower values indicate higher priority
 * (executed first).
 *
 * <p>
 * Priority ranges:
 * <ul>
 * <li>1-9: High priority - Framework critical (security, auth)</li>
 * <li>10-50: Normal priority - Application observers</li>
 * <li>51-100: Low priority - Logging, metrics, cleanup</li>
 * </ul>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RouteRendererObserverPriority {
  /**
   * Priority value.
   *
   * @return priority (default 10)
   */
  int value() default 10;
}
