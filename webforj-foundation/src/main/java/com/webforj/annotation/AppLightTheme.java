package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set the name of the default light theme to be used by the application when
 * the used theme is "system".
 *
 * <p>
 * The annotation can be used on the class level only and the class must extend `com.webforj.App` in
 * order for the annotation to be processed.
 * </p>
 *
 * <pre>
 * {@code @AppLightTheme("light")}
 * </pre>
 *
 * @see AppTheme
 * @see AppDarkTheme
 *
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@Documented
public @interface AppLightTheme {
  /**
   * The name of the default light theme to be used by the application.
   *
   * @return the name of the default light theme
   */
  String value() default "light";
}
