package com.webforj.mcp.event;

import java.util.EventObject;
import tools.jackson.databind.JsonNode;

/**
 * Carries the complete tool arguments the host delivered to the running application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpToolInputEvent extends EventObject {

  private final transient JsonNode arguments;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param arguments the complete tool arguments
   */
  public McpToolInputEvent(Object source, JsonNode arguments) {
    super(source);
    this.arguments = arguments;
  }

  /**
   * Returns the tool arguments.
   *
   * @return the arguments
   */
  public JsonNode getArguments() {
    return arguments;
  }
}
