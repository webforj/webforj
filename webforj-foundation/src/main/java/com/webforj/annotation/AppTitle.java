package com.webforj.annotation;

import com.webforj.Page;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set the title of the app.
 *
 * <p>
 * The annotation can be used on the class level only and the class must extend `com.webforj.App` in
 * order for the annotation to be processed.
 * </p>
 *
 * <pre>
 * {@code
 * &#64;AppTitle("My App")
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 */

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface AppTitle {

  /**
   * The title of the app.
   *
   * @return the title of the app
   **/
  String value();

  /**
   * The format of the title of the app.
   *
   * @return the format of the title of the app
   *
   * @see Page#setTitle(String, String, java.util.Map)
   **/
  String format() default Page.DEFAULT_TITLE_FORMAT;
}
