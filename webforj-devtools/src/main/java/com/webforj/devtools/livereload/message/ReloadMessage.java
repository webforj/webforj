package com.webforj.devtools.livereload.message;

/**
 * Message sent to trigger a browser reload.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class ReloadMessage extends LiveReloadMessage {

  /**
   * Creates a new reload message.
   */
  public ReloadMessage() {
    super("reload");
  }
}
