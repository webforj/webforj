package com.webforj.spring.scope.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * Scopes a bean to the webforJ session lifecycle.
 *
 * <p>
 * Session-scoped beans are created once per session and destroyed when the session ends. Each user
 * session gets its own instance, isolating user-specific data like authentication state or
 * preferences.
 * </p>
 *
 * <pre>{@code
 * &#64;Service
 * &#64;WebforjSessionScope
 * public class UserSessionService {
 *   private User currentUser;
 *   // Session-specific operations
 * }
 * }</pre>
 *
 * @see EnvironmentScope
 * @see RouteScope
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Scope(value = "webforj-session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public @interface WebforjSessionScope {
}
