package com.webforj.spring.devtools.message;

/**
 * Message sent when a client successfully connects to the WebSocket server.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class ConnectedMessage extends DevToolsMessage {

  /**
   * Creates a new connected message.
   */
  public ConnectedMessage() {
    super("connected");
  }
}
