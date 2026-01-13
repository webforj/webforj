package com.webforj;

import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjRuntimeException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Represents the incoming request with the various pieces of information provided with the incoming
 * request.
 *
 * @author Hyyan Abo Fakher
 * @since 23.01
 */
public final class Request {

  private Request() {}

  /**
   * Gets the current request instance.
   *
   * @return the current request instance
   */
  public static Request getCurrent() {
    String key = "com.webforj.request.Request.instance";
    if (ObjectTable.contains(key)) {
      return (Request) ObjectTable.get(key);
    }

    Request instance = new Request();
    ObjectTable.put(key, instance);

    return instance;
  }

  /**
   * Gets the request URL.
   *
   * @return The request URL
   * @since 24.02
   */
  public String getUrl() {
    try {
      return getEnvironment().getBBjAPI().getWebManager().getUrl();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get the URL.", e);
    }
  }

  /**
   * Gets the request protocol.
   *
   * @return The request protocol
   */
  public String getProtocol() {
    URL url;
    try {
      url = new URL(getUrl());
    } catch (MalformedURLException e) {
      return null;
    }

    return url.getProtocol();
  }

  /**
   * Gets the request host.
   *
   * @return The request host
   * @since 24.02
   */
  public String getHost() {
    URL url;
    try {
      url = new URL(getUrl());
    } catch (MalformedURLException e) {
      return null;
    }

    return url.getHost();
  }

  /**
   * Gets the request port.
   *
   * @return The request port
   * @since 24.02
   */
  public String getPort() {
    URL url;
    try {
      url = new URL(getUrl());
    } catch (MalformedURLException e) {
      return null;
    }

    return String.valueOf(url.getPort());
  }

  /**
   * Returns the value of the provided query parameter if present.
   *
   * @param key Key of the desired query parameter from the incoming request.
   * @return The value of the query parameter with the provided key, null if not present.
   * @since 24.02
   */
  public String getQueryParameter(String key) {
    try {
      return getEnvironment().getBBjAPI().getConfig().clientEnv(key, true);
    } catch (BBjException e) {
      return null;
    }
  }

  /**
   * Returns the IP address of the client that sent the request.
   *
   * @return the IP address of the client that sent the request
   * @since 24.02
   */
  public String getIPAddress() {
    try {
      return getEnvironment().getBBjAPI().getThinClient().getClientIPAddress();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client IP address", e);
    }
  }

  /**
   * Returns the public IP address of the client that sent the request.
   *
   * @return the public IP address of the client that sent the request
   * @since 24.02
   */
  public String getPublicIPAddress() {
    try {
      return getEnvironment().getBBjAPI().getThinClient().getPublicIPAddress();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client public IP address", e);
    }
  }

  /**
   * Returns the locale of the client that sent the request.
   *
   * @return the locale of the client that sent the request
   * @since 24.02
   */
  public Locale getLocale() {
    try {
      String jsLocale = getEnvironment().getBBjAPI().getThinClient().getClientLocale();
      return Locale.forLanguageTag(jsLocale);
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client locale", e);
    }
  }

  /**
   * Returns the preferred locales of the client that sent the request.
   *
   * @return the preferred locales of the client that sent the request
   * @since 24.02
   */
  public List<Locale> getPreferredLocales() {
    try {
      BBjVector locales = getEnvironment().getBBjAPI().getThinClient().getClientLocales();
      List<Locale> preferredLocales = new ArrayList<>();
      for (int i = 0; i < locales.size(); i++) {
        String jsLocale = locales.get(i).toString();
        preferredLocales.add(Locale.forLanguageTag(jsLocale));
      }

      return preferredLocales;
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client preferred locales", e);
    }
  }

  /**
   * Returns the time zone of the client that sent the request.
   *
   * @return the time zone of the client that sent the request
   * @since 24.02
   */
  public TimeZone getTimeZone() {
    try {
      return getEnvironment().getBBjAPI().getThinClient().getClientTimeZone();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client time zone", e);
    }
  }

  /**
   * Returns the operating system name from the client's machine.
   *
   * @return the operating system name from the client's machine
   * @since 24.02
   */
  public String getSystemName() {
    try {
      return getEnvironment().getBBjAPI().getThinClient().getClientOSName();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client system name", e);
    }
  }

  /**
   * Returns the operating system version from the client's machine.
   *
   * @return the operating system version from the client's machine
   * @since 24.02
   */
  public String getSystemVersion() {
    try {
      return getEnvironment().getBBjAPI().getThinClient().getClientOSVersion();
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to get client system version", e);
    }
  }

  Environment getEnvironment() {
    return Environment.getCurrent();
  }
}
