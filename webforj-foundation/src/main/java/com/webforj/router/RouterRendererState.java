package com.webforj.router;

import com.webforj.component.Component;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The {@code RouterRendererState} class represents the state of the router renderer.
 *
 * <p>
 * This class represents the state of the router renderer. The state contains the list of components
 * that were added and the list of components that were removed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public final class RouterRendererState {
  private final Component rendered;
  private final List<Component> added;
  private final List<Component> removed;

  /**
   * Constructs a new {@code RouterRendererState} instance.
   *
   * @param rendered the rendered component
   * @param added the list of components that were added
   * @param removed the list of components that were removed
   */
  public RouterRendererState(Component rendered, List<Component> added, List<Component> removed) {
    this.rendered = rendered;
    this.added = new ArrayList<>(added);
    this.removed = new ArrayList<>(removed);
  }

  /**
   * Gets the list of components that were added.
   *
   * @return the list of components that were added
   */
  public List<Component> getAdded() {
    return Collections.unmodifiableList(added);
  }

  /**
   * Gets the list of components that were removed.
   *
   * @return the list of components that were removed
   */
  public List<Component> getRemoved() {
    return Collections.unmodifiableList(removed);
  }

  /**
   * Gets the last rendered component.
   *
   * @return the last rendered component
   */
  public Component getRenderedComponent() {
    return rendered;
  }
}
