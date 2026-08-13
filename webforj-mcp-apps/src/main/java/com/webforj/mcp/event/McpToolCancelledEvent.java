package com.webforj.mcp.event;

import java.util.EventObject;

/**
 * Signals that the host cancelled the running tool call.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpToolCancelledEvent extends EventObject {

  private final String reason;

  /**
   * Creates the event.
   *
   * @param source the host connection the event arrived on
   * @param reason the reason the host named, may be {@code null}
   */
  public McpToolCancelledEvent(Object source, String reason) {
    super(source);
    this.reason = reason;
  }

  /**
   * Returns the reason the host named.
   *
   * @return the reason, {@code null} when the host named none
   */
  public String getReason() {
    return reason;
  }
}
