package com.webforj.spring.scope.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;

/**
 * Indicates that a bean should be scoped to the webforJ Environment lifecycle.
 *
 * <p>
 * Beans with this scope will be created once per request and destroyed when the Environment is
 * cleaned up at the end of the request. This is the webforJ equivalent of request scope.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Scope(value = "webforj-environment", proxyMode = ScopedProxyMode.TARGET_CLASS)
public @interface EnvironmentScope {
}
