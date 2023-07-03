package org.dwcj;

import com.basis.bbj.proxies.BBjThinClient;
import com.basis.startup.type.BBjException;
import org.dwcj.environment.ObjectTable;
import org.dwcj.environment.StringTable;
import org.dwcj.exceptions.DwcjRuntimeException;

/**
 * Gives access to the browsers cookie storage and allows adding/reading/removing values from it.
 */
public class CookieStorage extends AbstractWebStorage {

  private CookieStorage() throws BBjException {
    super(Environment.getCurrent().getBBjAPI().getThinClient(), WebStorageType.COOKIES);
  }

  CookieStorage(BBjThinClient thinClient) {
    super(thinClient, WebStorageType.COOKIES);
  }

  /**
   * Get the current CookieStorage instance.
   *
   * @return the current CookieStorage instance
   */
  public static CookieStorage getCurrent() {
    final String key = "dwcj.cookieStorage.instance";

    if (ObjectTable.contains(key)) {
      return (CookieStorage) ObjectTable.get(key);
    }

    try {
      final CookieStorage instance = new CookieStorage();
      ObjectTable.put(key, instance);
      return instance;
    } catch (BBjException e) {
      throw new DwcjRuntimeException("Failed to instantiate Cookiestorage.", e);
    }
  }

  /**
   * Sets the cookie path, defaults to window.location.pathname.
   *
   * @param path the path the cookies will be stored.
   */
  public void setCookiePath(String path) {
    StringTable.put("!COOKIE_PATH", path);
  }
}
