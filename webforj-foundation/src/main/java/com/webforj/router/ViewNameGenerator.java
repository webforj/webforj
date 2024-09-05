package com.webforj.router;

import com.webforj.component.Component;

/**
 * Generates view names based on the given component class.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public final class ViewNameGenerator {

  private ViewNameGenerator() {}

  /**
   * Derives the view name from the given class.
   *
   * @param component the component class to derive the view name from
   * @return the view name
   */
  public static String generate(Class<? extends Component> component) {
    String className = component.getSimpleName();

    if (className.equals("MainView") || className.equals("Main")) {
      return "";
    }

    className = className.replaceAll("View$", "");
    return className.toLowerCase();
  }
}
