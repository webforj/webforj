package com.webforj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.environment.ObjectTable;
import com.webforj.exceptions.WebforjRuntimeException;

/**
 * Represents a storage mechanism for managing
 * <a href="https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage">session
 * storage</a>.
 *
 * @author Timon Geisbauer, Hyyan Abo Fakher
 * @since 23.06
 */
public final class SessionStorage extends WebStorage {

  /**
   * Creates a new SessionStorage instance.
   *
   * @throws BBjException if the instance could not be created
   */
  private SessionStorage() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), Type.SESSION);
  }

  /**
   * Creates a new SessionStorage instance.
   *
   * @param thinClient the thin client to use
   */
  SessionStorage(BBjThinClient thinClient) {
    super(thinClient, Type.SESSION);
  }

  /**
   * Gets the current SessionStorage instance.
   *
   * @return the current SessionStorage instance
   */
  public static SessionStorage getCurrent() {
    final String key = ".webstorage.session.instance";

    if (ObjectTable.contains(key)) {
      return (SessionStorage) ObjectTable.get(key);
    }

    try {
      final SessionStorage instance = new SessionStorage();
      ObjectTable.put(key, instance);
      return instance;
    } catch (BBjException e) {
      throw new WebforjRuntimeException("Failed to instantiate SessionStorage.", e);
    }
  }
}
