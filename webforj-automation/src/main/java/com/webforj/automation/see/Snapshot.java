package com.webforj.automation.see;

import com.webforj.component.Component;

/**
 * Renders a snapshot of a component tree as text, one line per component.
 *
 * <p>
 * By default, a snapshot contains only the state held on the server and skips hidden components
 * with their children. Use {@link SnapshotOptions} to include the browser state and hidden
 * components.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class Snapshot {

  private Snapshot() {}

  /**
   * Renders a snapshot of the given component and its children with the default options.
   *
   * @param component the root component
   * @return the snapshot, or an empty string if the root component is hidden
   */
  public static String render(Component component) {
    return render(component, new SnapshotOptions());
  }

  /**
   * Renders a snapshot of the given component and its children.
   *
   * @param component the root component
   * @param options the snapshot options
   *
   * @return the snapshot, or an empty string if the root component is hidden and hidden components
   *         are not included
   */
  public static String render(Component component, SnapshotOptions options) {
    return SnapshotFormat.renderTree(component, options);
  }
}
