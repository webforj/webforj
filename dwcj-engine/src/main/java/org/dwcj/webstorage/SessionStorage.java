package org.dwcj.webstorage;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Gives access to the browsers session storage and allows adding/reading/removing values from it.
 */
public class SessionStorage extends AbstractWebStorage {

  private SessionStorage() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), WebStorageType.SESSION);
  }

  SessionStorage(BBjThinClient thinClient) {
    super(thinClient, WebStorageType.SESSION);
  }

  /**
   * Get the current SessionStorage instance.
   *
   * @return the current SessionStorage instance
   */
  public static SessionStorage getCurrent() {
    final String key = "dwcj.sessionStorage.instance";

    if (ObjectTable.contains(key)) {
      return (SessionStorage) ObjectTable.get(key);
    }

    try {
      final SessionStorage instance = new SessionStorage();
      ObjectTable.put(key, instance);
      return instance;
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to instantiate SessionStorage.", e);
    }
  }
}
