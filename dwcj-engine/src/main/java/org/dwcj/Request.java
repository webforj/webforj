package org.dwcj;

import org.dwcj.environment.ObjectTable;

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
    String key = "dwcj.request.instance";
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
    return Environment.getInstance().getDwcjHelper().getQueryParam(key);
  }

  public static String getCookie(String key) {
    return CookieStorage.getCurrent().get(key);
  }

}
