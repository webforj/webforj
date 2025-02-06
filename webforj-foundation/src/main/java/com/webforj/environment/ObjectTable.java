package com.webforj.environment;

import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import java.util.NoSuchElementException;

/**
 * Provides access to a map of key value pairs that is accessible at all program levels.
 *
 * @author Hyyan Abo Fakher
 */
public final class ObjectTable {

  /**
   * private constructor to prevent instantiation.
   */
  private ObjectTable() {}

  /**
   * Places a key/value pair into the table.
   *
   * @param key the key of the variable to access
   * @param value the contents to set in the field
   *
   * @return the value that was set
   */
  public static Object put(String key, Object value) {
    Environment.ifPresent(env -> env.getBBjAPI().getObjectTable().put(key, value));
    return value;
  }

  /**
   * Access a value in the Object Table.
   *
   * @param key the key of the variable to access
   * @return the contents of the field
   * @throws NoSuchElementException in case the object table entry does not exist
   */
  public static Object get(String key) {
    try {
      Environment env = Environment.getCurrent();
      if (env == null) {
        throw new NoSuchElementException("Element " + key + " does not exist!");
      }

      return env.getBBjAPI().getObjectTable().get(key);
    } catch (BBjException e) {
      throw new NoSuchElementException("Element " + key + " does not exist!");
    }
  }

  /**
   * Checks if the object table contains a key.
   *
   * @param key the key of the variable to check
   * @return true if the object table contains the key
   */
  public static boolean contains(String key) {
    try {
      Object result = ObjectTable.get(key);
      return result != null;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  /**
   * Clear an entry from the object table.
   *
   * @param key the key of the variable to remove
   */
  public static void clear(String key) {
    Environment.ifPresent(env -> env.getBBjAPI().getObjectTable().remove(key));
  }

  /**
   * Returns the number of key-value mappings in this map.
   *
   * @return the number of key-value mappings in this map
   */
  public static int size() {
    Environment env = Environment.getCurrent();
    if (env == null) {
      return 0;
    }

    return env.getBBjAPI().getObjectTable().size();
  }
}
