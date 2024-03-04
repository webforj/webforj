package com.webforj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.DwcjRuntimeException;

/**
 * Represents a storage mechanism for managing
 * <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage">local storage</a>.
 *
 * @author Timon Geisbauer, Hyyan Abo Fakher
 * @since 23.06
 */
public final class LocalStorage extends WebStorage {

  /**
   * Creates a new LocalStorage instance.
   *
   * @throws BBjException if the instance could not be created
   */
  private LocalStorage() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), Type.STORAGE);
  }

  /**
   * Creates a new LocalStorage instance.
   *
   * @param thinClient the thin client to use
   */
  LocalStorage(BBjThinClient thinClient) {
    super(thinClient, Type.STORAGE);
  }

  /**
   * Gets the current LocalStorage instance.
   *
   * @return the current LocalStorage instance
   */
  public static LocalStorage getCurrent() {
    final String key = "dwcj.webstorage.local.instance";

    if (ObjectTable.contains(key)) {
      return (LocalStorage) ObjectTable.get(key);
    }

    try {
      final LocalStorage instance = new LocalStorage();
      ObjectTable.put(key, instance);
      return instance;
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to instantiate LocalStorage.", e);
    }
  }
}
