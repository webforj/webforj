package com.webforj.mcp.event;

import java.util.EventObject;
import tools.jackson.databind.JsonNode;

/**
 * Carries the host context fields that changed while the application is displayed.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpHostContextChangedEvent extends EventObject {

  private final transient JsonNode changes;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param changes the changed context fields
   */
  public McpHostContextChangedEvent(Object source, JsonNode changes) {
    super(source);
    this.changes = changes;
  }

  /**
   * Returns the changed context fields.
   *
   * @return the changes
   */
  public JsonNode getChanges() {
    return changes;
  }
}
