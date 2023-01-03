package org.dwcj.annotations;

import java.lang.annotation.ElementType;
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
 * @author Hyyan Abo Fakher
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface AppMetaRepeatable {
  AppMeta[] value();
}
