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
 * Beans with this scope will be created once per browser window or tab and destroyed when the 
 * Environment is cleaned up (when the user closes the tab or the session expires). Each browser 
 * window or tab receives its own isolated instance of environment-scoped beans.
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
