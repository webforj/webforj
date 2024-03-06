package com.webforj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjRuntimeException;
import java.util.Map;

/**
 * Represents a storage mechanism for managing
 * <a href="https://developer.mozilla.org/en-US/docs/Web/API/Document/cookie">cookies</a>.
 *
 * @author Timon Geisbauer, Hyyan Abo Fakher
 * @since 23.06
 */
public final class CookieStorage extends WebStorage {

  /**
   * Creates a new CookieStorage instance.
   *
   * @throws BBjException if the instance could not be created
   */
  private CookieStorage() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), Type.COOKIES);
  }

  /**
   * Creates a new CookieStorage instance.
   *
   * @param thinClient the thin client to use
   */
  CookieStorage(BBjThinClient thinClient) {
    super(thinClient, Type.COOKIES);
  }

  /**
   * Gets the current CookieStorage instance.
   *
   * @return the current CookieStorage instance
   */
  public static CookieStorage getCurrent() {
    final String key = "dwcj.webstorage.cookie.instance";

    if (ObjectTable.contains(key)) {
      return (CookieStorage) ObjectTable.get(key);
    }

    try {
      final CookieStorage instance = new CookieStorage();
      ObjectTable.put(key, instance);
      return instance;
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to instantiate CookieStorage.", e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CookieStorage add(String key, String value, String attributes) {
    return (CookieStorage) super.add(key, value, attributes);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public CookieStorage add(Map<String, String> values, String attributes) {
    return (CookieStorage) super.add(values, attributes);
  }

  /**
   * Alias for {@link #add(String, String, String)}.
   */
  public CookieStorage setItem(String key, String value, String attributes) {
    return add(key, value, attributes);
  }
}
