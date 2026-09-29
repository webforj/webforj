package com.webforj.devtools.craftforj.utilities;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import java.util.ArrayList;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

/**
 * Walks the components the application shows.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ComponentTree {

  private ComponentTree() {}

  /**
   * Finds every component of the application.
   *
   * @return the components of every frame that are not destroyed, a parent before what it holds
   */
  public static List<Component> findAll() {
    List<Component> found = new ArrayList<>();
    Set<Component> seen = Collections.newSetFromMap(new IdentityHashMap<>());
    for (Frame frame : App.getFrames()) {
      collect(frame, seen, found);
    }

    return found;
  }

  /**
   * Finds every component a component holds.
   *
   * @param component the component to look below
   *
   * @return the components below it that are not destroyed, a parent before what it holds, the
   *         component itself left out
   */
  public static List<Component> findBelow(Component component) {
    List<Component> found = new ArrayList<>();
    collect(component, Collections.newSetFromMap(new IdentityHashMap<>()), found);

    return found.subList(1, found.size());
  }

  private static void collect(Component component, Set<Component> seen, List<Component> found) {
    // A view that was built again leaves its destroyed components behind in what held them, they
    // are no part of the application any more
    if (component == null || component.isDestroyed() || !seen.add(component)) {
      return;
    }

    found.add(component);
    if (component instanceof Composite<?>) {
      collect(getBoundComponent(component), seen, found);
    }

    if (component instanceof HasComponents container) {
      for (Component child : container.getComponents()) {
        collect(child, seen, found);
      }
    }
  }

  private static Component getBoundComponent(Component composite) {
    try {
      return ComponentUtil.getBoundComponent(composite);
    } catch (RuntimeException e) {
      // A composite that cannot hand out its component holds nothing the walk could reach
      return null;
    }
  }
}
