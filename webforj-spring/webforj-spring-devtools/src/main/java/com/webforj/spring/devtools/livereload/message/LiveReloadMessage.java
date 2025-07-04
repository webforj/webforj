package com.webforj.spring.devtools.livereload.message;

/**
 * Base class for all DevTools WebSocket messages.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class LiveReloadMessage {
  private final String type;

  /**
   * Creates a new DevTools message.
   *
   * @param type the message type
   */
  public LiveReloadMessage(String type) {
    this.type = type;
  }

  /**
   * Gets the message type.
   *
   * @return the message type
   */
  public String getType() {
    return type;
  }
}
