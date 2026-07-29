package com.webforj.devtools.craftforj.utilities;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Resolves the simple type names a component may be declared under in source code.
 *
 * <p>
 * A component declared in source may use its runtime class or any of its superclasses as the
 * declared type (e.g. {@code FlexLayout nav = new NavBar()}). The resulting set guards source
 * lookups against stale line numbers: a declaration whose type is unrelated to the component's
 * hierarchy is rejected instead of silently edited.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class ComponentTypeNames {

  private ComponentTypeNames() {}

  /**
   * Gets the simple names of the given class and all its superclasses below Object.
   *
   * @param componentClass the component's runtime class
   * @return the simple type names, most specific first
   */
  public static Set<String> of(Class<?> componentClass) {
    Set<String> names = new LinkedHashSet<>();

    for (Class<?> current = componentClass; current != null && current != Object.class; current =
        current.getSuperclass()) {
      String simpleName = current.getSimpleName();
      if (!simpleName.isEmpty()) {
        names.add(simpleName);
      }
    }

    return names;
  }
}
