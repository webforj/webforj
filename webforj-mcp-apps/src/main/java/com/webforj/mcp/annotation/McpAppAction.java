package com.webforj.mcp.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Publishes a method as a tool for the running view.
 *
 * <p>
 * The tool name combines the view name and the method name. The method may declare one object
 * parameter whose properties become the tool arguments. A method declared in a class listed by
 * {@link McpApp#actions()} must also accept the running view.
 * </p>
 *
 * <p>
 * A method may return {@code CallToolResult} directly. Other return values become structured
 * content, and a void method returns a confirmation.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface McpAppAction {

  /**
   * Sets the final segment of the published tool name.
   *
   * <p>
   * When blank, the segment derives from the method name.
   * </p>
   *
   * @return the name segment
   */
  String name() default "";

  /**
   * Sets the tool description.
   *
   * @return the tool description
   */
  String description();
}
