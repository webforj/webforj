package org.dwcj.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates a class to set a multiple meta tags in the app. 
 * 
 * The annotation can be used on the class level only and the class must extend
 * `org.dwcj.App` in order for the annotation to be processed.
 * 
 * <pre>
 * {@code
  *&#64;AppMetas({
  *    &#64;AppMeta(name = "description", content = "This is a test app"),
  *    &#64;AppMeta(name = "keywords", content = "test, app, dwc, bbj"),
  *    &#64;AppMeta(name = "theme-color", content = "#000000", attributes = {
  *        &#64;Attribute(name = "media", value = "(prefers-color-scheme: dark)"),
  *        &#64;Attribute(name = "name", value = "theme-color") })
  *})
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface AppMetas {
  AppMeta[] value();
}
