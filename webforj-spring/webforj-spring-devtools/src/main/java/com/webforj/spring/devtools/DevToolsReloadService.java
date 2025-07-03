package com.webforj.spring.devtools;

/**
 * Service responsible for triggering browser reloads through the WebSocket server.
 *
 * <p>
 * This service acts as a bridge between Spring components that detect changes and the WebSocket
 * server that communicates with browser clients. It provides a simple interface for triggering
 * reload commands.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class DevToolsReloadService {

  private static final System.Logger logger =
      System.getLogger(DevToolsReloadService.class.getName());
  private DevToolsServer webSocketServer;

  /**
   * Configures the WebSocket server to be used for reload operations.
   *
   * @param server the WebSocket server instance that manages browser connections
   */
  public void setWebSocketServer(DevToolsServer server) {
    this.webSocketServer = server;
  }

  /**
   * Sends a reload command to all connected browser clients.
   *
   * <p>
   * If the WebSocket server is not initialized, logs a warning and returns without error. This
   * method is safe to call even if no clients are connected.
   * </p>
   */
  public void triggerReload() {
    if (webSocketServer != null) {
      webSocketServer.sendReloadMessage();
    } else {
      logger.log(System.Logger.Level.WARNING,
          "WebSocket server not initialized, cannot trigger reload");
    }
  }
}
