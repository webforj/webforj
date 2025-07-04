package com.webforj.spring.devtools.livereload.message;

/**
 * Message sent in response to heartbeat ping.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class HeartbeatAckMessage extends LiveReloadMessage {

  /**
   * Creates a new heartbeat acknowledgment message.
   */
  public HeartbeatAckMessage() {
    super("heartbeat-ack");
  }
}
