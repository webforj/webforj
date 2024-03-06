package com.webforj;

import com.webforj.environment.ObjectTable;
import com.webforj.webstorage.CookieStorage;

/**
 * Represents the incoming request with the various pieces of information provided with the incoming
 * request.
 */
public final class Request {

  private Request() {}

  /**
   * Get the current request instance.
   *
   * @return the current request instance
   */
  public static Request getCurrent() {
    String key = ".request.instance";
    if (ObjectTable.contains(key)) {
      return (Request) ObjectTable.get(key);
    }

    Request instance = new Request();
    ObjectTable.put(key, instance);

    return instance;
  }

  /**
   * Returns the value of the provided query parameter if present.
   *
   * @param key Key of the desired query parameter from the incoming request.
   * @return The value of the query parameter with the provided key, null if not present.
   */
  public static String getQueryParam(String key) {
    return Environment.getCurrent().getWeforjHelper().getQueryParam(key);
  }

  /**
   * Returns the value of the stored cookie.
   *
   * @param key the key of the cookie
   * @return the value for the given key, null if key is not found
   */
  public static String getCookie(String key) {
    return CookieStorage.getCurrent().get(key);
  }

}
