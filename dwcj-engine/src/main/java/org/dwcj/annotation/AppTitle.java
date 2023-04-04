package org.dwcj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set the title of the DWC app.
 *
 * The annotation can be used on the class level only and the class must extend `org.dwcj.App` in
 * order for the annotation to be processed.
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
   * The title of the app
   *
   * @return the title of the app
   **/
  String value();
}
