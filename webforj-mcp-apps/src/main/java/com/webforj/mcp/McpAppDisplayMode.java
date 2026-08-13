package com.webforj.mcp;

import com.fasterxml.jackson.annotation.JsonCreator;

/**
 * The display modes an application can ask an MCP host for.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public enum McpAppDisplayMode {

  /**
   * The application shares the conversation surface, the default mode of the protocol.
   */
  INLINE("inline"),

  /**
   * The application fills the host window.
   */
  FULLSCREEN("fullscreen"),

  /**
   * The application floats above the conversation.
   */
  PIP("pip");

  private final String value;

  McpAppDisplayMode(String value) {
    this.value = value;
  }

  /**
   * Returns the wire value of the mode.
   *
   * @return the wire value
   */
  public String getValue() {
    return value;
  }

  /**
   * Returns the mode carrying the given wire value.
   *
   * @param value the wire value
   * @return the matching mode, {@link #INLINE} when nothing matches since the protocol names inline
   *         as the host default
   */
  @JsonCreator
  public static McpAppDisplayMode fromValue(String value) {
    for (McpAppDisplayMode mode : values()) {
      if (mode.value.equalsIgnoreCase(value)) {
        return mode;
      }
    }

    return INLINE;
  }
}
