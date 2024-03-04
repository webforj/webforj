package com.webforj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.webforj.exceptions.DwcjRuntimeException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Abstract implementation for similarly named Browser API. WebStorage may be useful to save some
 * data that you want to be stored on the client side, instead of e.g. database on the server. An
 * example could be certain application settings that the same users might want to have set
 * differently based on their device.
 *
 * @see CookieStorage
 * @see LocalStorage
 * @see SessionStorage
 *
 * @author Timon Geisbauer, Hyyan Abo Fakher
 * @since 23.06
 */
// @formatter:off
public abstract sealed class WebStorage permits CookieStorage, LocalStorage, SessionStorage {
  // @formatter:on

  /**
   * The type of available web storages.
   */
  enum Type {
    /**
     * Maps to browser cookies with a 30-day expiration.
     */
    COOKIES(BBjThinClient.USER_PROPERTIES_COOKIES),

    /**
     * Maps to browser local storage.
     */
    STORAGE(BBjThinClient.USER_PROPERTIES_STORAGE),

    /**
     * Maps to browser session storage.
     */
    SESSION(BBjThinClient.USER_PROPERTIES_SESSION);

    private long value;

    private Type(long value) {
      this.value = value;
    }

    /**
     * Gets the value of the enum.
     *
     * @return the value of the enum
     */
    public long getValue() {
      return value;
    }
  }

  private final BBjThinClient thinClient;
  private final Type type;

  /**
   * Creates a new instance of the WebStorage.
   *
   * @param thinClient the BBj thin client
   * @param type the type of the storage
   */
  protected WebStorage(BBjThinClient thinClient, Type type) {
    this.thinClient = thinClient;
    this.type = type;
  }

  /**
   * When passed a key name and value, will add that key to the storage, or update that key's value
   * if it already exists.
   *
   * @param key the key of the entry
   * @param value the value of the entry
   *
   * @return the current instance
   */
  public WebStorage add(String key, String value) {
    add(key, value, BBjThinClient.SAME_SITE_DEFAULT);
    return this;
  }

  /**
   * When passed a key name and value, will add that key to the storage, or update that key's value
   * if it already exists.
   *
   * @param values a map of key/value pairs to be stored
   *
   * @return the current instance
   */
  public WebStorage add(Map<String, String> values) {
    return add(values, BBjThinClient.SAME_SITE_DEFAULT);
  }

  /**
   * When passed a key name and value, will add that key to the storage, or update that key's value
   * if it already exists.
   *
   * @param key the key of the entry
   * @param value the value of the entry
   * @param attributes the attributes of the entry
   *
   * @return the current instance
   */
  protected WebStorage add(String key, String value, String attributes) {
    try {
      thinClient.setUserProperty(type.getValue(), Optional.ofNullable(attributes).orElse(""), key,
          value);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add '" + key + "' with value '" + value
          + "' to the '" + getClass().getSimpleName() + "'.", e);
    }

    return this;
  }

  /**
   * When passed a key name and value, will add that key to the storage, or update that key's value
   * if it already exists.
   *
   * @param values a map of key/value pairs to be stored
   * @param attributes the attributes of the entry
   *
   * @return the current instance
   */
  protected WebStorage add(Map<String, String> values, String attributes) {
    try {
      thinClient.setUserProperties(type.getValue(), attributes, values);
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to add values '" + values.toString() + "' to the '"
          + getClass().getSimpleName() + "'.", e);
    }

    return this;
  }

  /**
   * Alias for {@link #add(String, String)}.
   */
  public WebStorage setItem(String key, String value) {
    return add(key, value);
  }

  /**
   * When passed a key name, will return that key's value.
   *
   * @param key the key name
   * @return the value of the key
   */
  public String get(String key) {
    try {
      return thinClient.getUserProperty(type.getValue(), key);
    } catch (BBjException e) {
      return "";
    }
  }

  /**
   * Tries to read with the given keys.
   *
   * @param keys a collection of keys
   *
   * @return a map with keys and the values
   */
  public Map<String, String> get(List<String> keys) {
    try {
      return thinClient.getUserProperties(type.getValue(), keys);
    } catch (BBjException e) {
      return Collections.emptyMap();
    }
  }

  /**
   * Alias for {@link #get(String)}.
   */
  public String getItem(String key) {
    return get(key);
  }

  /**
   * When passed a key name, will remove that key from the storage.
   *
   * @param keys the keys to remove
   */
  public void remove(String... keys) {
    for (String key : keys) {
      try {
        thinClient.setUserProperty(type.getValue(), key, null);
      } catch (BBjException e) {
        // pass
      }
    }
  }

  /**
   * Alias for {@link #remove(String...)}.
   */
  public void removeItem(String... keys) {
    remove(keys);
  }

  /**
   * When invoked, will empty all keys out of the storage.
   */
  public void clear() {
    try {
      thinClient.clearUserProperties(type.getValue());
    } catch (BBjException e) {
      // pass
    }
  }
}
