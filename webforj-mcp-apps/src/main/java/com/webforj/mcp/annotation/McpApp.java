package com.webforj.mcp.annotation;

import com.webforj.mcp.McpAppDisplayMode;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Exposes a routed view to MCP hosts as a tool.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface McpApp {

  /**
   * Describes what the view shows so the host can decide when to open it.
   *
   * @return the view description
   */
  String description();

  /**
   * Names the generated tool.
   *
   * <p>
   * When left empty the name derives from the route path.
   * </p>
   *
   * @return the tool name
   */
  String name() default "";

  /**
   * Declares the input schema of the generated tool, as a JSON Schema document.
   *
   * <p>
   * The document is published verbatim, so everything JSON Schema expresses is available, nested
   * objects, arrays, enums and constraints included. The host validates and fills the declared
   * arguments and delivers them to the running view. When left empty the tool takes no input.
   * </p>
   *
   * @return the JSON Schema document
   */
  String inputSchema() default "";

  /**
   * Declares the class the input schema of the generated tool derives from.
   *
   * <p>
   * The schema is generated from the class structure. Property descriptions come from Jackson's
   * {@code @JsonPropertyDescription} and required properties from
   * {@code @JsonProperty(required = true)}, so the class describes its arguments the same way any
   * Jackson bound type does. Declare either this or {@link #inputSchema()}, never both.
   * </p>
   *
   * @return the class the input schema derives from
   */
  Class<?> input() default Void.class;

  /**
   * Names classes that hold actions and opening input methods for the view.
   *
   * <p>
   * The framework resolves each class through the conceiver before calling its methods.
   * </p>
   *
   * @return the classes carrying the actions of the view
   */
  Class<?>[] actions() default {};

  /**
   * Declares the display mode the view asks the host for when it opens.
   *
   * <p>
   * The view requests the declared mode from the host as soon as it opens. The host may keep
   * another mode, and the application can request a different mode at any time through
   * {@code McpHost.requestDisplayMode}. The default asks for fullscreen, where a full application
   * has the room it renders for. A view that wants the default presentation of the host declares
   * {@link McpAppDisplayMode#INLINE}, the default mode of the protocol.
   * </p>
   *
   * @return the display mode the view asks for
   */
  McpAppDisplayMode displayMode() default McpAppDisplayMode.FULLSCREEN;
}
