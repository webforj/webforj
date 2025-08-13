package com.webforj.spring.scope;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * Alias for {@link WebforjEnvironmentScope}.
 *
 * <p>
 * This annotation provides a familiar name for Spring developers. It has the exact same behavior as
 * {@link WebforjEnvironmentScope}, creating beans that live for the duration of a single request.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 * @see WebforjEnvironmentScope
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Scope(value = "webforj-environment", proxyMode = ScopedProxyMode.TARGET_CLASS)
public @interface WebforjRequestScope {
}
