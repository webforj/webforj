package com.webforj.environment;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Provides access to the HttpSession attributes.
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
public final class SessionObjectTable {

  private SessionObjectTable() {}

  /**
   * Places a key/value pair into the session.
   *
   * @param key the attribute name
   * @param value the attribute value to set
   *
   * @return the value that was set
   * @throws IllegalStateException if no session accessor is available
   */
  public static Object put(String key, Object value) {
    HttpSession.Accessor accessor = getAccessor();
    if (accessor == null) {
      throw new IllegalStateException("No session available. Session is only available when "
          + "running in a servlet environment with Jakarta Servlet 6.1+ support.");
    }

    accessor.access(session -> session.setAttribute(key, value));
    return value;
  }

  /**
   * Access a value in the session.
   *
   * @param key the attribute name to access
   * @return the attribute value
   *
   * @throws NoSuchElementException if the session attribute does not exist
   * @throws IllegalStateException if no session accessor is available
   */
  public static Object get(String key) {
    HttpSession.Accessor accessor = getAccessor();
    if (accessor == null) {
      throw new IllegalStateException("No session available. Session is only available when "
          + "running in a servlet environment with Jakarta Servlet 6.1+ support.");
    }

    AtomicReference<Object> valueRef = new AtomicReference<>();
    accessor.access(session -> valueRef.set(session.getAttribute(key)));

    Object value = valueRef.get();
    if (value == null) {
      throw new NoSuchElementException("Session attribute '" + key + "' does not exist!");
    }

    return value;
  }

  /**
   * Checks if the session contains an attribute.
   *
   * @param key the attribute name to check
   * @return true if the session contains the attribute, false otherwise
   */
  public static boolean contains(String key) {
    HttpSession.Accessor accessor = getAccessor();
    if (accessor == null) {
      return false;
    }

    AtomicReference<Boolean> existsRef = new AtomicReference<>(false);
    accessor.access(session -> existsRef.set(session.getAttribute(key) != null));
    return existsRef.get();
  }

  /**
   * Removes an attribute from the session.
   *
   * @param key the attribute name to remove
   */
  public static void clear(String key) {
    HttpSession.Accessor accessor = getAccessor();
    if (accessor != null) {
      accessor.access(session -> session.removeAttribute(key));
    }
  }

  /**
   * Returns the session ID if available.
   *
   * @return the session ID, or null if no session is available
   */
  public static String getSessionId() {
    HttpSession.Accessor accessor = getAccessor();
    if (accessor == null) {
      return null;
    }

    AtomicReference<String> idRef = new AtomicReference<>();
    accessor.access(session -> idRef.set(session.getId()));
    return idRef.get();
  }

  /**
   * Checks if a session is available.
   *
   * @return true if a session accessor is available, false otherwise
   */
  public static boolean isAvailable() {
    return getAccessor() != null;
  }

  /**
   * Gets the HttpSession.Accessor from the current Environment.
   *
   * @return the HttpSession.Accessor if available, null otherwise
   */
  private static HttpSession.Accessor getAccessor() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return null;
    }

    return env.getSessionAccessor();
  }
}
