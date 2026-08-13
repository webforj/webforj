package com.webforj.mcp.event;

import io.modelcontextprotocol.spec.McpSchema.CallToolResult;
import java.util.EventObject;

/**
 * Carries the finished tool result the host delivered to the running application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpToolResultEvent extends EventObject {

  private final transient CallToolResult result;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param result the tool call result as the host sent it
   */
  public McpToolResultEvent(Object source, CallToolResult result) {
    super(source);
    this.result = result;
  }

  /**
   * Returns the tool call result.
   *
   * @return the result
   */
  public CallToolResult getResult() {
    return result;
  }
}
