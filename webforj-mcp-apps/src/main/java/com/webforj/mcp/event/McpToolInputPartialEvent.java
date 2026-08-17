package com.webforj.mcp.event;

import java.util.EventObject;
import tools.jackson.databind.JsonNode;

/**
 * Carries partial tool arguments the host streams while the model is still writing them.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpToolInputPartialEvent extends EventObject {

  private final transient JsonNode arguments;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param arguments the partial tool arguments
   */
  public McpToolInputPartialEvent(Object source, JsonNode arguments) {
    super(source);
    this.arguments = arguments;
  }

  /**
   * Returns the partial tool arguments.
   *
   * @return the arguments
   */
  public JsonNode getArguments() {
    return arguments;
  }
}
