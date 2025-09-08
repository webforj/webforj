package com.webforj.router.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Indicates that a route allows anonymous access.
 *
 * <p>
 * Routes marked with this annotation do not require authentication and are accessible to all users,
 * including anonymous users. This annotation takes precedence over secure-by-default configuration.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.04
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AnonymousAccess {
}
