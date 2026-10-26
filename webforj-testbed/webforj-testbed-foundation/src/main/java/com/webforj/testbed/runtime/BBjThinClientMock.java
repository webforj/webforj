package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.startup.type.BBjVector;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The runtime mock backing the client of the context under test.
 *
 * <p>
 * The mock answers client questions with deterministic defaults, a locale of {@code en-US}, the UTC
 * time zone and the loopback address. Tests can change the locale and time zone through
 * {@link #setClientLocale(String)} and {@link #setClientTimeZone(TimeZone)}. User properties are
 * stored per property category so that web storage and cookie storage keep separate state.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjThinClientMock extends RuntimeMock {
  private final BBjClientFileSystemMock fileSystem = new BBjClientFileSystemMock();
  private final Map<Long, Map<String, String>> userProperties = new ConcurrentHashMap<>();
  private String clientLocale = "en-US";
  private TimeZone clientTimeZone = TimeZone.getTimeZone("UTC");

  /**
   * Returns the file system of the client.
   *
   * @return the client file system mock
   */
  BBjClientFileSystemMock getFileSystem() {
    return fileSystem;
  }

  /**
   * Sets the locale reported to the application under test.
   *
   * @param clientLocale the locale as a language tag
   */
  void setClientLocale(String clientLocale) {
    this.clientLocale = clientLocale;
  }

  /**
   * Sets the time zone reported to the application under test.
   *
   * @param clientTimeZone the time zone
   */
  void setClientTimeZone(TimeZone clientTimeZone) {
    this.clientTimeZone = clientTimeZone;
  }

  /**
   * Returns the user properties stored under the given category.
   *
   * @param category the property category
   *
   * @return the properties of the category
   */
  Map<String, String> getUserProperties(long category) {
    return propertiesOf(category);
  }

  private Map<String, String> propertiesOf(long category) {
    return userProperties.computeIfAbsent(category, key -> new ConcurrentHashMap<>());
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "getClientFileSystem" -> fileSystem.getProxy(BBjClientFileSystem.class);
      case "getClientLocale" -> clientLocale;
      case "getClientLocales" -> new BBjVector(List.of(clientLocale));
      case "getClientTimeZone" -> clientTimeZone;
      case "setClientTimeZone" -> {
        clientTimeZone = (TimeZone) args[0];
        yield null;
      }
      case "getClientIPAddress", "getPublicIPAddress" -> "127.0.0.1";
      case "getClientOSName" -> System.getProperty("os.name", "");
      case "getClientOSVersion" -> System.getProperty("os.version", "");
      case "setUserProperty" -> handleSetUserProperty(args);
      case "getUserProperty" -> handleGetUserProperty(args);
      case "setUserProperties" -> handleSetUserProperties(args);
      case "getUserProperties" -> handleGetUserProperties(args);
      case "clearUserProperties" -> {
        if (args.length == 1 && args[0] instanceof Long category) {
          propertiesOf(category).clear();
        } else {
          userProperties.clear();
        }
        yield null;
      }
      default -> UNHANDLED;
    };
  }

  private Object handleSetUserProperty(Object[] args) {
    if (args.length == 2) {
      putOrRemove(0L, (String) args[0], (String) args[1]);
    } else if (args.length == 3) {
      putOrRemove((Long) args[0], (String) args[1], (String) args[2]);
    } else if (args.length == 4) {
      putOrRemove((Long) args[0], (String) args[2], (String) args[3]);
    }

    return null;
  }

  private void putOrRemove(long category, String key, String value) {
    if (value == null) {
      propertiesOf(category).remove(key);
    } else {
      propertiesOf(category).put(key, value);
    }
  }

  private Object handleGetUserProperty(Object[] args) {
    if (args.length == 1) {
      return propertiesOf(0L).get(args[0]);
    }

    return propertiesOf((Long) args[0]).get(args[1]);
  }

  private Object handleSetUserProperties(Object[] args) {
    if (args.length == 1) {
      propertiesOf(0L).putAll(asStringMap(args[0]));
    } else if (args.length == 2) {
      propertiesOf((Long) args[0]).putAll(asStringMap(args[1]));
    } else if (args.length == 3) {
      propertiesOf((Long) args[0]).putAll(asStringMap(args[2]));
    }

    return null;
  }

  private Object handleGetUserProperties(Object[] args) {
    long category = args.length > 0 && args[0] instanceof Long categoryArg ? categoryArg : 0L;
    Map<String, String> properties = new LinkedHashMap<>(propertiesOf(category));

    Object lastArgument = args.length > 0 ? args[args.length - 1] : null;
    if (lastArgument instanceof Collection<?> requestedKeys) {
      properties.keySet().retainAll(requestedKeys);
    }

    return properties;
  }

  @SuppressWarnings("unchecked")
  private Map<String, String> asStringMap(Object value) {
    return (Map<String, String>) value;
  }
}
