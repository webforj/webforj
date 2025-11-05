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
 * @since 25.10
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

  /**
   * Error code to use when access is denied.
   *
   * <p>
   * This code will be passed as the reason in the query parameter when redirecting to the access
   * denied page. If not specified, defaults to "expression_denied".
   * </p>
   *
   * @return error code for access denial
   */
  String code() default "expression_denied";
}
