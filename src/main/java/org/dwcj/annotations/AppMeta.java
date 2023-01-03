package org.dwcj.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to set the meta tags of the app. It can be used
 * multiple times to set multiple meta tags. The attributes of the meta tag can
 * be set using the {@link MetaAttribute} annotation. The attributes can be
 * repeated multiple times to set multiple attributes but the attributes are
 * optional.
 * 
 * <p>
 * Example:
 * 
 * <pre>
 * {@code
 * &#64;AppMeta(name = "author", content = "Hyyan Abo Fakher")
 * &#64;AppMeta(name = "description", content = "This is a test app")
 * &#64;AppMeta(name = "keywords", content = "test, app")
 * &#64;AppMeta(name="viewport", content="width=device-width,initial-scale=1,user-scalable=no")
 * &#64;AppMeta(name = "custom name", content = "custom content", attributes = {
 *   &#64;MetaAttribute(name = "custom-attribute", value = "custom attribute value"),
 *   &#64;MetaAttribute(name = "custom-attribute2", value = "custom attribute value2")
 * })
 * </pre>
 * 
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Repeatable(AppMetaRepeatable.class)
public @interface AppMeta {
  /** The name of the meta tag */
  String name();

  /** The content of the meta tag */
  String content();

  /** The attributes of the meta tag */
  MetaAttribute[] attributes() default {};
}
