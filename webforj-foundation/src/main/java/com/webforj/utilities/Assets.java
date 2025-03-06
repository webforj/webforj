package com.webforj.utilities;

import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.Environment;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.router.RouterUtils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Base64;
import java.util.stream.Collectors;

/**
 * A helper class to deal with assets in the class path.
 *
 * @author Hyyan Abo Fakher
 */
public class Assets {

  /**
   * Enum defining the formats for content representation.
   */
  public enum ContentFormat {
    /** Indicates that the content should be returned as plain text. */
    PLAIN_TEXT,

    /** Indicates that the content should be returned as base64 encoded string. */
    BASE64
  }

  private Assets() {}

  /**
   * Get the content of a resource as a String.
   *
   * @param path The path to the resource
   * @param format The format of the content
   *
   * @return The content of the resource as a String
   *
   * @throws IllegalArgumentException if the path is null or empty
   * @throws WebforjRuntimeException if an error occurred while reading the resource
   */
  public static String contentOf(String path, ContentFormat format) {
    ClassLoader classLoader = Environment.getCurrent().getClass().getClassLoader();
    try (InputStream is = classLoader.getResourceAsStream(path)) {
      if (is == null) {
        throw new IllegalArgumentException("Resource not found: " + path);
      }

      if (format == ContentFormat.BASE64) {
        return Base64.getEncoder().encodeToString(is.readAllBytes());
      } else {
        try (InputStreamReader isr = new InputStreamReader(is);
            BufferedReader reader = new BufferedReader(isr)) {
          return reader.lines().collect(Collectors.joining(System.lineSeparator()));
        }
      }
    } catch (IOException e) {
      throw new WebforjRuntimeException(e);
    }
  }

  /**
   * Gets the content of a resource as a String.
   *
   * @param path The path to the resource
   *
   * @return The content of the resource as a String
   *
   * @throws IllegalArgumentException if the path is null or empty
   * @throws WebforjRuntimeException if an error occurred while reading the resource
   */
  public static String contentOf(String path) {
    return contentOf(path, ContentFormat.PLAIN_TEXT);
  }

  /**
   * Get the URL of the Jetty Web Server's files directory.
   *
   * @return The URL of the Jetty Web Server's files directory.
   */
  public static String getWebServerFilesUrl() {
    String url = "/files/" + App.getApplicationName() + "/";
    boolean withBBjService = Environment.isRunningWithBBjServices();

    if (!withBBjService) {
      url = "static/";
      Config config = Environment.getCurrent().getConfig();
      String assetsDirProp = "webforj.assetsDir";
      String assetsDir = config.hasPath(assetsDirProp) && !config.getIsNull(assetsDirProp)
          ? config.getString(assetsDirProp)
          : null;
      if (assetsDir != null && !assetsDir.isEmpty()) {
        url = RouterUtils.normalizePath(assetsDir);
      }
    }

    String context = Environment.getContextPath();
    String fullUrl = context + "/" + url;
    return RouterUtils.normalizePath(fullUrl);
  }

  /**
   * Check if the given url starts with the <code>webserver://</code> protocol or not.
   *
   * <p>
   * For example: <code>webserver://static/css/foo.css</code> will return true
   * <code>http://example.com/static/css/foo.css</code> will return false
   * </p>
   *
   * @param url The url
   * @return true if the url is an jetty url, false otherwise
   */
  public static boolean isWebServerUrl(String url) {
    return url.toLowerCase().startsWith("webserver://") || url.toLowerCase().startsWith("ws://");
  }

  /**
   * Get the url from the given webserver url.
   *
   * <p>
   * For example: <code>webserver://static/css/foo.css</code> will return
   * <code>${APP_URL}/files/${APP_NAME}/static/css/foo.css</code>
   * <code>http://example.com/static/css/foo.css</code> will throw an IllegalArgumentException
   * </p>
   *
   * @param url The url
   * @throws IllegalArgumentException if the url is not a webserver url
   */
  public static String resolveWebServerUrl(String url) {
    if (!isWebServerUrl(url)) {
      throw new IllegalArgumentException(
          "URL does not being the 'webserver://' or 'ws://'  protocol: " + url);
    }

    String fullUrl = getWebServerFilesUrl() + "/"
        + url.replaceAll("(?i)webserver://", "").replaceAll("(?i)ws://", "").trim();

    return fullUrl.replaceAll("(?<!\\w+:/?)//+", "/");
  }

  /**
   * Check if the given url is an local url or not.
   *
   * @param url The url
   * @return true if the url is an local url, false otherwise
   */
  public static boolean isContextUrl(String url) {
    return url.toLowerCase().startsWith("context://");
  }

  /**
   * Get the url from the given local url.
   *
   * @param url The url
   *
   * @return The url
   * @throws IllegalArgumentException if the url is not an local url
   */
  public static String resolveContextUrl(String url) {
    if (!isContextUrl(url)) {
      throw new IllegalArgumentException("URL does not being the \"context://\" protocol: " + url);
    }

    return url.replaceAll("(?i)context://", "").trim();
  }

  /**
   * Retrieves the URL for the icons endpoint, which is used to serve icons.
   *
   * <p>
   * By default, webforj serves icons from the <code>resources/icons/</code> directory. You can
   * change the endpoint name by setting the <code>webforj.iconsDir</code> property in the webforj
   * configuration file. The default endpoint is <code>icons/</code>.
   * </p>
   *
   * <p>
   * The icons endpoint accepts the icon file name as a path parameter with a specified size. For
   * example, <code>icons://icon-144x144.png</code> will return the icon with a size of 144x144. The
   * base image should be placed in the <code>resources/icons/</code> directory and named
   * <code>icon.png</code>.
   * </p>
   *
   * <p>
   * Additionally, you can add padding or a background color to the icon using the following query
   * parameters:
   * <ul>
   * <li><code>padding</code>: The padding around the icon, specified as a number between 0 and 1.
   * For example, <code>icons://icon-144x144.png?padding=.3</code> adds a 30% padding around the
   * icon.</li>
   * <li><code>background</code>: The background color of the icon, specified as a valid hex color.
   * For example, <code>icons://icon-144x144.png?background=ffffff</code> adds a white background to
   * the icon.</li>
   * </ul>
   * </p>
   *
   * @return The URL of the icons endpoint.
   * @since 24.22
   */
  public static String getIconsEndpoint() {
    boolean withBbjService = Environment.isRunningWithBBjServices();
    if (withBbjService) {
      return "";
    }

    String url = "icons/";
    Config config = Environment.getCurrent().getConfig();
    String assetsDirProp = "webforj.iconsDir";
    String assetsDir = config.hasPath(assetsDirProp) && !config.getIsNull(assetsDirProp)
        ? config.getString(assetsDirProp)
        : null;
    if (assetsDir != null && !assetsDir.isEmpty()) {
      url = RouterUtils.normalizePath(assetsDir);
    }

    String context = Environment.getContextPath();
    String fullUrl = context + "/" + url;
    return RouterUtils.normalizePath(fullUrl);
  }

  /**
   * Check if the given url is an icons url or not.
   *
   * @param url The url
   * @return true if the url is an icons url, false otherwise
   *
   * @see #getIconsEndpoint()
   * @since 24.22
   */
  public static boolean isIconsUrl(String url) {
    return url.toLowerCase().startsWith("icons://");
  }

  /**
   * Get the url from the given icons url.
   *
   * @param url The url
   *
   * @return The url
   * @throws IllegalArgumentException if the url is not an icons url
   *
   * @see #getIconsEndpoint()
   * @since 24.22
   */
  public static String resolveIconsUrl(String url) {
    if (!isIconsUrl(url)) {
      throw new IllegalArgumentException("URL does not being the \"icons://\" protocol: " + url);
    }

    boolean withBbjService = Environment.isRunningWithBBjServices();
    if (withBbjService) {
      throw new WebforjRuntimeException(
          "Icons protocol is not supported when running with BBj Services");
    }

    return getIconsEndpoint() + url.replaceAll("(?i)icons://", "").trim();
  }

  /**
   * Get the file name from a given path.
   *
   * @param path The path to extract the file name from
   * @return The file name
   */
  public static String getFileName(String path) {
    if (path == null || path.isEmpty()) {
      return "";
    }

    int queryIndex = path.indexOf('?');
    if (queryIndex != -1) {
      path = path.substring(0, queryIndex);
    }

    int lastSlashIndex = path.lastIndexOf('/');
    return lastSlashIndex == -1 ? path : path.substring(lastSlashIndex + 1);
  }

  /**
   * Get the file extension from a given file name.
   *
   * @param fileName The file name to extract the extension from
   * @return The file extension
   */
  public static String getFileExtension(String fileName) {
    if (fileName == null || fileName.isEmpty() || !fileName.contains(".")) {
      return "";
    }

    int queryIndex = fileName.indexOf('?');
    if (queryIndex != -1) {
      fileName = fileName.substring(0, queryIndex);
    }

    int lastDotIndex = fileName.lastIndexOf('.');
    return lastDotIndex == -1 ? "" : fileName.substring(lastDotIndex);
  }
}
