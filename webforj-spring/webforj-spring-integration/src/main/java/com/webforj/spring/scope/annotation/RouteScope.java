package com.webforj.spring.scope.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * Indicates that a bean should be scoped to the webforJ route hierarchy.
 *
 * <p>
 * Route-scoped beans are shared across all components within the same route hierarchy (parent and
 * child routes). When navigating to a different route hierarchy, new bean instances are created.
 * </p>
 *
 * <p>
 * Example usage:
 *
 * <pre>
 * {@code @RouteScope}
 * {@code @Component}
 * public class UserSessionService {
 *   // This service instance is shared within a route hierarchy
 * }
 * </pre>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 * @see RouteScopeProcessor
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Scope(value = "webforj-route", proxyMode = ScopedProxyMode.TARGET_CLASS)
public @interface RouteScope {
}
