package com.webforj.devtools.craftforj.utilities;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

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

    Set<String> visited = new HashSet<>();
    for (Frame frame : App.getFrames()) {
      Optional<Component> result = findById(frame, id, visited);
      if (result.isPresent()) {
        return result;
      }
    }

    return Optional.empty();
  }

  private static Optional<Component> findById(Component component, String targetId,
      Set<String> visited) {
    String componentId = component.getComponentId();

    // Skip components with null componentId
    if (componentId == null) {
      // Still traverse children if this is a container
      if (component instanceof HasComponents container) {
        for (Component child : container.getComponents()) {
          Optional<Component> result = findById(child, targetId, visited);
          if (result.isPresent()) {
            return result;
          }
        }
      }
      return Optional.empty();
    }

    if (visited.contains(componentId)) {
      return Optional.empty();
    }
    visited.add(componentId);

    if (componentId.equals(targetId)) {
      return Optional.of(component);
    }

    if (component instanceof HasComponents container) {
      for (Component child : container.getComponents()) {
        Optional<Component> result = findById(child, targetId, visited);
        if (result.isPresent()) {
          return result;
        }
      }
    }

    return Optional.empty();
  }
}
