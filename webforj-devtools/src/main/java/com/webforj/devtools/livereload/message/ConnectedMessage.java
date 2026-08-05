package com.webforj.devtools.livereload.message;

/**
 * Message sent when a client successfully connects to the WebSocket server.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class ConnectedMessage extends LiveReloadMessage {

  private final String hotswapTool;
  private final String hotswapLevel;

  /**
   * Creates a new connected message without a hotswap state.
   */
  public ConnectedMessage() {
    this(null, null);
  }

  /**
   * Creates a new connected message carrying the hotswap state of the run.
   *
   * @param hotswapTool the attached hotswap tool, {@code hotswapAgent} or {@code jrebel}, or
   *        {@code null} when none is attached
   * @param hotswapLevel the depth of the class updates the tool applies, {@code full} or
   *        {@code limited}, or {@code null} when no tool is attached
   *
   * @since 26.02
   */
  public ConnectedMessage(String hotswapTool, String hotswapLevel) {
    super("connected");
    this.hotswapTool = hotswapTool;
    this.hotswapLevel = hotswapLevel;
  }

  /**
   * Gets the attached hotswap tool.
   *
   * @return the tool, or {@code null} when none is attached
   *
   * @since 26.02
   */
  public String getHotswapTool() {
    return hotswapTool;
  }

  /**
   * Gets the depth of the class updates the tool applies.
   *
   * @return the level, or {@code null} when no tool is attached
   *
   * @since 26.02
   */
  public String getHotswapLevel() {
    return hotswapLevel;
  }
}
