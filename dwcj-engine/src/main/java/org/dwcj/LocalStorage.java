package org.dwcj;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import org.dwcj.environment.ObjectTable;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Gives access to the browsers local storage and allows adding/reading/removing values from it.
 */
public class LocalStorage extends AbstractWebStorage {

  private LocalStorage() throws BBjException {
    super(Environment.getInstance().getBBjAPI().getThinClient(), WebStorageType.STORAGE);
  }

  public LocalStorage(BBjThinClient thinClient) {
    super(thinClient, WebStorageType.STORAGE);
  }

  /**
   * Get the current LocalStorage instance.
   *
   * @return the current LocalStorage instance
   */
  public static LocalStorage getCurrent() {
    final String key = "dwcj.localStorage.instance";

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
