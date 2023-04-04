package org.dwcj.environment;

import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

import java.util.NoSuchElementException;

/**
 * Provides access to the STBL String Table.
 *
 * The STBL String Table is persistent, and global scope in this thread Values can be changed
 * programmatically or set in the config file, e.g.:
 *
 * <pre>
 * SET COMPANY=Acme
 * </pre>
 *
 * Then you can access the value with
 *
 * {@code String val = StringTable.get("COMPANY")}
 */
public final class StringTable {

  /**
   * private constructor to prevent instantiation
   */
  private StringTable() {}

  /**
   * Checks if the string table contains a key
   *
   * @param key the key of the variable to check
   * @return true if the string table contains the key
   */
  public static boolean contains(String key) {
    try {
      StringTable.get(key);
      return true;
    } catch (NoSuchElementException e) {
      return false;
    }
  }

  /**
   * Access a value in the string table.
   *
   * @param key the key of the variable to access
   * @return the contents of the field
   *
   * @throws NoSuchElementException in case the string table entry does not exist
   */
  public static String get(String key) {
    try {
      return Environment.getInstance().getBBjAPI().getStbl(key);
    } catch (BBjException e) {
      throw new NoSuchElementException("Element " + key + " does not exist!");
    }
  }

  /**
   * Places a key/value pair into the string table.
   *
   * @param key the key of the variable to access
   * @param value the contents to set in the field
   *
   * @return the value just set
   */
  public static String put(String key, String value) {
    try {
      Environment.getInstance().getBBjAPI().setStbl(key, value);
    } catch (BBjException e) {
      Environment.logError(e);
    }
    return value;
  }

  /**
   * Clear an entry from the string table
   *
   * @param key the key of the variable to remove
   */
  public static void clear(String key) {
    try {
      Environment.getInstance().getBBjAPI().setStbl("!CLEAR", key);
    } catch (BBjException e) {
      Environment.logError(e);
    }
  }
}
