package com.webforj.spring.devtools.browser;

import java.awt.Desktop;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.net.InetAddress;
import java.net.URI;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.boot.web.context.WebServerInitializedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.core.env.Environment;

/**
 * Launches the OS browser when the application starts in development mode.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public class BrowserLauncher {
  private static final Logger LOG = System.getLogger(BrowserLauncher.class.getName());
  private final Environment environment;
  private Integer serverPort;
  private static boolean browserOpened = false;

  BrowserLauncher(Environment environment) {
    this.environment = environment;
  }

  /**
   * Sets the server port when the web server is initialized.
   *
   * @param event the web server initialized event
   */
  @EventListener
  public void onWebServerInitialized(WebServerInitializedEvent event) {
    this.serverPort = event.getWebServer().getPort();
  }

  /**
   * Opens the browser when the application is ready.
   *
   * @param event the application ready event
   */
  @EventListener
  public void onApplicationReady(ApplicationReadyEvent event) {
    if (!environment.getProperty("webforj.devtools.browser.open", Boolean.class, false)) {
      return;
    }

    // Check if browser was already opened for this application instance
    if (browserOpened) {
      LOG.log(Level.DEBUG, "Browser already opened for this application, skipping");
      return;
    }

    String url = buildUrl();

    try {
      if (Desktop.isDesktopSupported()) {
        Desktop desktop = Desktop.getDesktop();
        if (desktop.isSupported(Desktop.Action.BROWSE)) {
          desktop.browse(new URI(url));
          browserOpened = true;
          LOG.log(Level.INFO, "Opened browser at: {0}", url);
        }
      }
    } catch (Exception e) {
      LOG.log(Level.WARNING, "Could not open browser", e);
    }
  }

  String buildUrl() {
    if (serverPort == null) {
      serverPort = environment.getProperty("server.port", Integer.class, 8080);
    }

    // Get the servlet mapping from webforj configuration
    String servletMapping = environment.getProperty("webforj.servlet-mapping", "/*");

    // Build the URL path
    String path = "";
    if (!servletMapping.equals("/*")) {
      // Remove the wildcard patterns
      path = servletMapping.replace("/*", "").replace("*", "");
      if (!path.startsWith("/")) {
        path = "/" + path;
      }
    }

    boolean sslEnabled = environment.getProperty("server.ssl.enabled", Boolean.class, false);
    String protocol = sslEnabled ? "https" : "http";

    // Determine host to use
    String host = "localhost";
    String hostType = environment.getProperty("webforj.devtools.browser.host", "localhost");

    switch (hostType.toUpperCase()) {
      case "HOSTNAME":
        try {
          host = InetAddress.getLocalHost().getHostName();
        } catch (Exception e) {
          LOG.log(Level.WARNING, "Could not determine machine hostname, using localhost", e);
        }
        break;
      case "IP-ADDRESS":
        try {
          host = InetAddress.getLocalHost().getHostAddress();
        } catch (Exception e) {
          LOG.log(Level.WARNING, "Could not determine machine IP address, using localhost", e);
        }
        break;
      default:
        break;
    }

    return String.format("%s://%s:%d%s", protocol, host, serverPort, path);
  }
}
