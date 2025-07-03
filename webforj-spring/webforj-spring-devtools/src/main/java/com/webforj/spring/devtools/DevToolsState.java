package com.webforj.spring.devtools;

/**
 * Static holder for DevTools state that persists across Spring DevTools restarts.
 *
 * <p>
 * This class is loaded by the base classloader (not the restart classloader) to ensure the
 * WebSocket server instance survives application restarts. This allows browser connections to
 * remain active while the application code reloads.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
public class DevToolsState {
  private static DevToolsServer webSocketServer;
  private static volatile boolean hasStartedOnce = false;

  /**
   * Retrieves the persistent WebSocket server instance.
   *
   * @return the WebSocket server instance, or null if not yet initialized
   */
  public static DevToolsServer getWebSocketServer() {
    return webSocketServer;
  }

  /**
   * Stores the WebSocket server instance for persistence across restarts.
   *
   * @param server the WebSocket server to persist
   */
  public static void setWebSocketServer(DevToolsServer server) {
    webSocketServer = server;
  }

  /**
   * Determines whether the application has completed its initial startup.
   *
   * @return true if application has started at least once, false on first startup
   */
  public static boolean hasStartedOnce() {
    return hasStartedOnce;
  }

  /**
   * Records that the application has completed its initial startup. Should be called once during
   * the first application start.
   */
  public static void markStarted() {
    hasStartedOnce = true;
  }
}
