package com.webforj.devtools.livereload.message;

/**
 * Message sent when the server is about to restart.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class RestartingMessage extends LiveReloadMessage {

  /**
   * Creates a new restarting message.
   */
  public RestartingMessage() {
    super("restarting");
  }
}
