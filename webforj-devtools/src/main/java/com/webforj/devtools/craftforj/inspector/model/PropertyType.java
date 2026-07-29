package com.webforj.devtools.craftforj.inspector.model;

/**
 * Defines the property type which determines both the UI control and the client-server contract.
 *
 * <p>
 * Each property type has a strict contract for how the client sends values to the server:
 * </p>
 *
 * <ul>
 * <li>{@link #TEXT} - Client sends: {@code "string value"}</li>
 * <li>{@link #BOOLEAN} - Client sends: {@code true} or {@code false}</li>
 * <li>{@link #NUMBER} - Client sends: {@code 123} or {@code 45.67}</li>
 * <li>{@link #SELECT} - Client sends: {@code "selected_option"} (from provided options)</li>
 * <li>{@link #LIST} - Client sends: {@code {action: "add|remove", item: "x"}}</li>
 * <li>{@link #SIZE} - Client sends: {@code "100px"} (CSS size string)</li>
 * <li>{@link #ICON} - Client sends: {@code "pool:name"} (icon pool and name)</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum PropertyType {

  /**
   * Text input field.
   *
   * <p>
   * Contract: Client sends a string value directly.
   * </p>
   *
   * <pre>
   * "hello world"
   * </pre>
   */
  TEXT,

  /**
   * Boolean checkbox.
   *
   * <p>
   * Contract: Client sends a boolean value directly.
   * </p>
   *
   * <pre>
   * true
   * </pre>
   */
  BOOLEAN,

  /**
   * Numeric input field.
   *
   * <p>
   * Contract: Client sends a number value directly.
   * </p>
   *
   * <pre>
   * 42
   * </pre>
   */
  NUMBER,

  /**
   * Select dropdown with predefined options.
   *
   * <p>
   * Contract: Client sends a string value from the provided options.
   * </p>
   *
   * <pre>
   * "OPTION_NAME"
   * </pre>
   */
  SELECT,

  /**
   * List editor (for class names, tags).
   *
   * <p>
   * Contract: Client sends an action object with item.
   * </p>
   *
   * <pre>
   * {
   *   "action": "add" | "remove",
   *   "item": "itemValue"
   * }
   * </pre>
   */
  LIST,

  /**
   * Size editor for CSS dimensions (height, width, min-height, etc.).
   *
   * <p>
   * Displays a text input with a unit selector dropdown (px, em, rem, %, vh, vw, etc.). The client
   * combines the value and unit into a CSS size string.
   * </p>
   *
   * <p>
   * Contract: Client sends a string value directly (the combined size).
   * </p>
   *
   * <pre>
   * "100px"
   * "50%"
   * "2rem"
   * </pre>
   */
  SIZE,

  /**
   * Icon picker for icon pool and name.
   *
   * <p>
   * Contract: Client sends a string value combining the pool and the icon name separated by a
   * colon.
   * </p>
   *
   * <pre>
   * "tabler:home"
   * "feather:bell"
   * "dwc:calendar"
   * </pre>
   */
  ICON
}
