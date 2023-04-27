package org.dwcj.utilities;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;
import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * A helper class to deal with assets in the class path.
 *
 * @author Hyyan Abo Fakher
 */
public class Assets {

  private Assets() {}

  /**
   * Get the content of a resource as a String.
   *
   * @param path The path to the resource
   * @return The content of the resource as a String
   *
   * @throws IllegalArgumentException if the path is null or empty
   * @throws DwcjRuntimeException if an error occurred while reading the resource
   */
  public static String contentOf(String path) {
    ClassLoader classLoader = Environment.getInstance().getClass().getClassLoader();
    try (InputStream is = classLoader.getResourceAsStream(path)) {
      if (is == null) {
        throw new IllegalArgumentException("Resource not found: " + path);
      }

      try (InputStreamReader isr = new InputStreamReader(is);
          BufferedReader reader = new BufferedReader(isr)) {
        return reader.lines().collect(Collectors.joining(System.lineSeparator()));
      }
    } catch (IOException e) {
      throw new DwcjRuntimeException(e);
    }
  }

  /**
   * Get the URL of the Jetty Web Server's files directory.
   *
   * @return The URL of the Jetty Web Server's files directory.
   */
  public static String getWebServerFilesUrl() {
    IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();
    Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

    return (String) helper.invokeMethod(instance, "getWebServerFilesURL", null);
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
    return url.toLowerCase().startsWith("webserver://");
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
          "URL does not being the \"webserver://\" protocol: " + url);
    }

    String fullUrl = getWebServerFilesUrl() + App.getApplicationName() + "/"
        + url.replaceAll("(?i)webserver://", "").trim();

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
}
