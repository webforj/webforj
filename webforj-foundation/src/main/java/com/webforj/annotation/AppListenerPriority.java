package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the priority for an {@link com.webforj.AppLifecycleListener} implementation.
 *
 * <p>
 * This annotation is optional and used to control the order in which lifecycle listeners are
 * invoked. Listeners with lower priority values are invoked before listeners with higher priority
 * values. If this annotation is not present, a default priority of 10 is used.
 * </p>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>{@code
 * &#64;AppListenerPriority(5)
 * public class HighPriorityListener implements AppLifecycleListener {
 *   &#64;Override
 *   public void onDidRun(App app) {
 *     // This will be called before listeners with higher priority values
 *   }
 * }
 * }</pre>
 *
 * <p>
 * Common priority values:
 * </p>
 * <ul>
 * <li>1-9: High priority (framework-level listeners)</li>
 * <li>10-50: Normal priority (application-level listeners)</li>
 * <li>50-100: Low priority (logging, metrics, etc.)</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 *
 * @see com.webforj.AppLifecycleListener
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AppListenerPriority {

  /**
   * The priority value for this listener.
   *
   * <p>
   * Lower values indicate higher priority. The default value is 10 if this annotation is not
   * present on the listener class.
   * </p>
   *
   * @return the priority value
   */
  int value();
}
