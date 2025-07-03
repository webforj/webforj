package com.webforj.spring.devtools.message;

/**
 * Message sent in response to heartbeat ping.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class HeartbeatAckMessage extends DevToolsMessage {

  /**
   * Creates a new heartbeat acknowledgment message.
   */
  public HeartbeatAckMessage() {
    super("heartbeat-ack");
  }
}
