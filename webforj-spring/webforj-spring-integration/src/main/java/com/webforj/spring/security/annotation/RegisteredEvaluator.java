package com.webforj.spring.security.annotation;

import com.webforj.router.security.RouteSecurityEvaluator;
import com.webforj.spring.security.SpringRouteSecurityManager;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.stereotype.Component;

/**
 * Marks a class as a route security evaluator that should be automatically discovered and
 * registered with the {@link SpringRouteSecurityManager}.
 *
 * <p>
 * Classes annotated with {@code @RegisteredEvaluator} must implement {@link RouteSecurityEvaluator}
 * and will be automatically registered during Spring's component scanning.
 * </p>
 *
 * <p>
 * The priority determines the order in which evaluators are executed, with lower values executing
 * first.
 * </p>
 *
 * <p>
 * Recommended priority ranges:
 * </p>
 * <ul>
 * <li>0 - 9: Reserved for core framework evaluators</li>
 * <li>10+: Custom user evaluators</li>
 * </ul>
 *
 * <p>
 * Example usage:
 * </p>
 *
 * <pre>{@code @RegisteredEvaluator(priority = 10)
 * public class CustomAccessEvaluator implements RouteSecurityEvaluator {
 *   // Implementation
 * }
 * }</pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.10
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Component
public @interface RegisteredEvaluator {

  /**
   * The priority of this evaluator.
   *
   * <p>
   * Lower values execute first.
   * </p>
   *
   * <ul>
   * <li>Framework evaluators: 0 to 9</li>
   * <li>Custom evaluators: typically 10 or higher</li>
   * </ul>
   *
   * @return the evaluator priority
   */
  int priority();
}
