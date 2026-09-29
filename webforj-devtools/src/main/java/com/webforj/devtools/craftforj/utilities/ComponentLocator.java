package com.webforj.devtools.craftforj.utilities;

import com.webforj.component.Component;
import java.util.Optional;

/**
 * Utility class for locating components within the application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ComponentLocator {

  private ComponentLocator() {
    // utility class
  }

  /**
   * Finds a component by its server-side component ID.
   *
   * @param id the component ID to search for
   * @return an Optional containing the component if found, empty otherwise
   */
  public static Optional<Component> findById(String id) {
    if (id == null || id.isEmpty()) {
      return Optional.empty();
    }

    return ComponentTree.findAll().stream()
        .filter(component -> id.equals(component.getComponentId())).findFirst();
  }
}
