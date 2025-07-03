package com.webforj.spring.devtools.message;

/**
 * Message sent to trigger a browser reload.
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class ReloadMessage extends DevToolsMessage {

  /**
   * Creates a new reload message.
   */
  public ReloadMessage() {
    super("reload");
  }
}
