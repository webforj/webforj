package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to set the default dark theme name to use when the
 * application theme is set to "system" and the system is in dark mode, the
 * default value is "dark"
 * 
 * <pre>
 * &#064;AppDarkTheme("dark")
 * public class MyApplication extends App {
 *   &#064;Override
 *   public void run() throws DwcException {
 *     msgbox("this is a test");
 *   }
 * }
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Inherited
@Documented
public @interface AppDarkTheme {
  String value() default "dark";
}
