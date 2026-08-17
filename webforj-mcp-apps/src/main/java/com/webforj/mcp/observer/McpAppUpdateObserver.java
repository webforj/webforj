package com.webforj.mcp.observer;

import com.webforj.mcp.event.McpAppUpdateEvent;
import io.modelcontextprotocol.spec.McpSchema.CallToolResult;

/**
 * Handles updates sent to a running view through its update tool.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@FunctionalInterface
public interface McpAppUpdateObserver {

  /**
   * Handles an update tool call while the view is on screen.
   *
   * @param event the update, carrying the tool name and its arguments
   * @return the result returned to the tool caller
   */
  CallToolResult onMcpAppUpdate(McpAppUpdateEvent event);
}
