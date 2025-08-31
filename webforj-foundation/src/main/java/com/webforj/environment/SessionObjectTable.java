package com.webforj.environment;

import com.webforj.Environment;
import jakarta.servlet.http.HttpSession;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Provides key/value pair storage in the HTTP session.
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
    Optional<HttpSession.Accessor> accessor = getAccessor();
    if (accessor.isEmpty()) {
      throw new IllegalStateException("No session available. Session is only available when "
          + "running in a servlet environment with Jakarta Servlet 6.1+ support.");
    }

    accessor.get().access(session -> session.setAttribute(key, value));
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
    Optional<HttpSession.Accessor> accessor = getAccessor();
    if (accessor.isEmpty()) {
      throw new IllegalStateException("No session available. Session is only available when "
          + "running in a servlet environment with Jakarta Servlet 6.1+ support.");
    }

    AtomicReference<Object> valueRef = new AtomicReference<>();
    accessor.get().access(session -> valueRef.set(session.getAttribute(key)));

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
    Optional<HttpSession.Accessor> accessor = getAccessor();
    if (accessor.isEmpty()) {
      return false;
    }

    AtomicReference<Boolean> existsRef = new AtomicReference<>(false);
    accessor.get().access(session -> existsRef.set(session.getAttribute(key) != null));
    return existsRef.get();
  }

  /**
   * Removes an attribute from the session.
   *
   * @param key the attribute name to remove
   */
  public static void clear(String key) {
    Optional<HttpSession.Accessor> accessor = getAccessor();
    accessor.ifPresent(a -> a.access(session -> session.removeAttribute(key)));
  }

  /**
   * Returns the number of key-value mappings in the session.
   *
   * @return the number of key-value mappings in the session
   */
  public static int size() {
    Optional<HttpSession.Accessor> accessor = getAccessor();
    if (accessor.isEmpty()) {
      return 0;
    }

    AtomicReference<Integer> sizeRef = new AtomicReference<>(0);
    accessor.get()
        .access(session -> sizeRef.set(Collections.list(session.getAttributeNames()).size()));
    return sizeRef.get();
  }

  /**
   * Gets the HttpSession.Accessor from the current Environment.
   *
   * @return an Optional containing the HttpSession.Accessor if available
   */
  private static Optional<HttpSession.Accessor> getAccessor() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return Optional.empty();
    }

    return env.getSessionAccessor();
  }
}
