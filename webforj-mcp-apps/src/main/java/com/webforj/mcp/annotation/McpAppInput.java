package com.webforj.mcp.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Identifies the method that receives arguments used to open a view.
 *
 * <p>
 * The method declares one object parameter whose properties define the opening tool arguments. The
 * framework calls the method after rendering the view. A method declared in a class listed by
 * {@link McpApp#actions()} must also accept the running view. A view using this annotation cannot
 * also set {@link McpApp#input()} or {@link McpApp#inputSchema()}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface McpAppInput {
}
