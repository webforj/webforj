package com.webforj.spring.security.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Authorizes access to routes using Spring Expression Language (SpEL) expressions.
 *
 * <p>
 * Example expressions:
 * <ul>
 * <li>{@code "hasRole('ADMIN')"} - requires ADMIN role</li>
 * <li>{@code "hasAnyRole('ADMIN', 'MANAGER')"} - requires either ADMIN or MANAGER role</li>
 * </ul>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RouteAccess {

  /**
   * SpEL expression for security evaluation.
   *
   * @return security expression
   */
  String value();
}
