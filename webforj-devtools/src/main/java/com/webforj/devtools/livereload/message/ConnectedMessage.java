package com.webforj.devtools.livereload.message;

/**
 * Message sent when a client successfully connects to the WebSocket server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class ConnectedMessage extends LiveReloadMessage {

  /**
   * Creates a new connected message.
   */
  public ConnectedMessage() {
    super("connected");
  }
}
