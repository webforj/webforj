package org.dwcj.environment;

import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

/**
 * Provides access to a map of key value pairs that is accessible at all program
 * levels.
 * 
 * @author Hyyan Abo Fakher
 */
public final class ObjectTable {

  /**
   * private constructor to prevent instantiation
   */
  private ObjectTable() {
  }

  /**
   * Places a key/value pair into the table.
   * 
   * @param key   the key of the variable to access
   * @param value the contents to set in the field
   */
  public static void put(String key, Object value) {
    Environment.getInstance().getBBjAPI().getObjectTable().put(key, value);
  }

  /**
   * Access a value in the Object Table.
   * 
   * @param key the key of the variable to access
   * @return the contents of the field
   */
  public static Object get(String key) {
    try {
      return Environment.getInstance().getBBjAPI().getObjectTable().get(key);
    } catch (BBjException e) {
      Environment.logError(e);
    }

    return key;
  }

  /**
   * Checks if the object table contains a key
   * 
   * @param key the key of the variable to check
   * @return true if the object table contains the key
   */
  public static boolean contains(String key) {
    Object value = get(key);
    return value != null;
  }

  /**
   * Clear an entry from the object table
   * 
   * @param key the key of the variable to remove
   */
  public static void clear(String key) {
    Environment.getInstance().getBBjAPI().getObjectTable().remove(key);
  }

  /**
   * Returns the number of key-value mappings in this map.
   * 
   * @return the number of key-value mappings in this map
   */
  public static int size() {
    return Environment.getInstance().getBBjAPI().getObjectTable().size();
  }
}
