package com.webforj.router;


/**
 * Utility class for router.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class RouterUtils {

  private RouterUtils() {
    // no-op
  }

  /**
   * Normalizes the path by removing redundant slashes and ensuring that the path starts with a
   * single slash.
   *
   * @param path the path to normalize
   * @return the normalized path
   */
  public static String normalizePath(String path) {
    if (path == null || path.isBlank()) {
      return "";
    }

    String result = path.replaceAll("/{2,}", "/");
    result = result.startsWith("/") ? result : "/" + result;

    return result;
  }
}
