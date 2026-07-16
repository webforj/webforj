package com.webforj.devtools.livereload;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.Request;
import com.webforj.annotation.AppListenerPriority;
import com.webforj.exceptions.WebforjRuntimeException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Injects the reload client script into every page when live reload is on.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
@AppListenerPriority(0)
public class LiveReloadScriptInjector implements AppLifecycleListener {

  private static final System.Logger logger =
      System.getLogger(LiveReloadScriptInjector.class.getName());
  private static final String SCRIPT_RESOURCE =
      "/META-INF/resources/webforj/devtools-reload-client.min.js";

  private String cachedScript;

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
    inject(LiveReloadOptions.from(getConfig()), Page.getCurrent(), Request.getCurrent());
  }

  void inject(LiveReloadOptions options, Page page, Request request) {
    if (!options.isEnabled() || page == null || request == null) {
      return;
    }

    try {
      String reloadScript = getReloadScript();
      if (reloadScript == null) {
        logger.log(System.Logger.Level.ERROR, "Failed to load webforJ livereload script resource");

        return;
      }

      String url = resolveWebsocketUrl(request.getProtocol(), request.getHost(),
          options.getWebsocketPort(), options.getWebsocketPath());
      page.addInlineJavaScript(composeScript(url, options.getHeartbeatInterval(), reloadScript),
          true);
      logger.log(System.Logger.Level.DEBUG, "webforJ livereload script injected into page");
    } catch (WebforjRuntimeException e) {
      // The request only becomes unreadable when its session is already ending, usually because
      // a restart began while this page was still starting. The page is about to be replaced, so
      // the injection is skipped quietly.
      logger.log(System.Logger.Level.DEBUG,
          "webforJ livereload script not injected, the session ended while the page was starting",
          e);
    } catch (Exception e) {
      logger.log(System.Logger.Level.WARNING, "Failed to inject webforJ livereload script", e);
    }
  }

  /**
   * Builds the reload socket url for the page, dropping any port already carried by the host.
   *
   * @param protocol the request protocol
   * @param host the request host, optionally carrying a port
   * @param port the reload socket port
   * @param path the reload socket path
   * @return the socket url the client connects to
   */
  static String resolveWebsocketUrl(String protocol, String host, int port, String path) {
    String scheme = "https".equals(protocol) ? "wss" : "ws";
    int colonIndex = host.indexOf(':');
    if (colonIndex > 0) {
      host = host.substring(0, colonIndex);
    }

    return String.format("%s://%s:%d%s", scheme, host, port, path);
  }

  /**
   * Joins the client configuration and the reload client script into a single inline script.
   *
   * @param websocketUrl the socket url the client connects to
   * @param heartbeatInterval the client heartbeat interval in milliseconds
   * @param reloadScript the reload client script body
   * @return the inline script added to the page
   */
  static String composeScript(String websocketUrl, int heartbeatInterval, String reloadScript) {
    String config = String.format("window.webforjDevToolsConfig = {" + "  enabled: true,"
        + "  websocketUrl: '%s'," + "  heartbeatInterval: %d," + "  reconnectDelay: 1000,"
        + "  maxReconnectAttempts: 10" + "};", websocketUrl, heartbeatInterval);

    return config + "\n" + reloadScript;
  }

  String getReloadScript() {
    if (cachedScript != null) {
      return cachedScript;
    }

    try (InputStream is = getClass().getResourceAsStream(SCRIPT_RESOURCE)) {
      if (is == null) {
        return null;
      }
      cachedScript = new String(is.readAllBytes(), StandardCharsets.UTF_8);

      return cachedScript;
    } catch (Exception e) {
      logger.log(System.Logger.Level.ERROR, "Failed to load webforJ livereload script", e);
    }

    return null;
  }

  private static Config getConfig() {
    Environment env = Environment.getCurrent();

    return env != null ? env.getConfig() : null;
  }
}
