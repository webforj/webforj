package org.dwcj.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to set the title of the app.
 * 
 * <p>
 * Example:
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
public @interface AppTitle {

    /** The title of the app */
    String value();
}
