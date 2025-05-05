package com.webforj.spring.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * Convenience alias for {@link org.springframework.stereotype.Component} to prevent conflicts with
 * {@link com.webforj.component.Component}.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@Target(java.lang.annotation.ElementType.TYPE)
@Retention(java.lang.annotation.RetentionPolicy.RUNTIME)
@Documented
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public @interface SpringComponent {
  /**
   * The value may indicate a suggestion for a logical component name, to be turned into a Spring
   * bean name in case of an autodetected component.
   *
   * @return the suggested component name, if any (or empty String otherwise)
   *
   * @see Component#value()
   */
  String value() default "";
}
