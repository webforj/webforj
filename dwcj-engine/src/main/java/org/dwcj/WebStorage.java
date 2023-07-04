package org.dwcj;

import java.util.Collection;
import java.util.Map;

/**
 * Defines methods for basic WebStorage interaction.
 */
public interface WebStorage {

  /**
   * Adds an entry to the storage.
   *
   * @param key the key of the entry
   * @param value the value stored
   */
  public void add(String key, String value);

  /**
   * Iterates through the map and adds all entries to the storage.
   *
   * @param values a map with the keys and values
   */
  public void add(Map<String, String> values);


  /**
   * Removes an entry for the given key.
   *
   * @param key the key of the value to be removed
   */
  public void remove(String... key);

  /**
   * Tries to read the value for the given key.
   *
   * @param key the key to access the stored value
   * @return the stored value
   */
  public String get(String key);

  /**
   * Tries to read with the given keys.
   *
   * @param keys a collection of keys
   * @return a map with keys and the values
   */
  public Map<String, String> get(Collection<String> keys);

  /**
   * Clears all entries in the storage.
   */
  public void clear();
}
