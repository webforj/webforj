package com.webforj.component.element;

import com.webforj.environment.ObjectTable;
import java.util.HashSet;
import java.util.Set;

/**
 * Remembers, per session, the element names the browser has reported as defined.
 *
 * <p>
 * A custom element name that is defined once stays defined for the life of the page, so an element
 * created later with the same name is defined as soon as the browser creates it. Knowing this lets
 * an element skip asking the browser to report its definition. The names are kept in the object
 * table of the session and end with it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.10
 */
final class DefinedElementNames {
  private static final String KEY = "com.webforj.component.element.DefinedElementNames";

  private DefinedElementNames() {}

  /**
   * Notes that the browser of the current session has defined the given element name.
   *
   * @param name the element name
   */
  static void add(String name) {
    if (name != null) {
      names().add(name);
    }
  }

  /**
   * Checks whether the browser of the current session has defined the given element name.
   *
   * @param name the element name
   * @return {@code true} when the name is known to be defined in the current session
   */
  static boolean contains(String name) {
    return name != null && names().contains(name);
  }

  @SuppressWarnings("unchecked")
  private static Set<String> names() {
    if (ObjectTable.contains(KEY)) {
      return (Set<String>) ObjectTable.get(KEY);
    }

    Set<String> names = new HashSet<>();
    ObjectTable.put(KEY, names);

    return names;
  }
}
