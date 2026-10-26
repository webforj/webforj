package com.webforj.automation.see;

/**
 * Represents the options of a snapshot.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SnapshotOptions {

  /**
   * The default maximum number of data lines rendered under a node.
   */
  public static final int DEFAULT_MAX_WIDTH = 10;

  private int maxWidth = DEFAULT_MAX_WIDTH;
  private boolean browserState;
  private boolean hiddenComponents;
  private boolean composites;

  /**
   * Returns the maximum number of data lines rendered under a node.
   *
   * @return the maximum width
   */
  public int getMaxWidth() {
    return maxWidth;
  }

  /**
   * Sets the maximum number of data lines rendered under a node, such as the rows of a table or the
   * items of a list.
   *
   * @param maxWidth the maximum width, at least one
   * @return the options itself
   *
   * @throws IllegalArgumentException if the maximum width is less than one
   */
  public SnapshotOptions setMaxWidth(int maxWidth) {
    if (maxWidth < 1) {
      throw new IllegalArgumentException("The snapshot width must be at least one");
    }

    this.maxWidth = maxWidth;

    return this;
  }

  /**
   * Checks if the snapshot includes the browser state.
   *
   * @return true if the snapshot includes the browser state
   */
  public boolean isBrowserState() {
    return browserState;
  }

  /**
   * Sets whether the snapshot includes the browser state, such as the value entered by the user,
   * the text of an element or the focus. Without it, the snapshot contains only the state held on
   * the server and sends no request to the browser.
   *
   * @param browserState true to include the browser state
   * @return the options itself
   */
  public SnapshotOptions setBrowserState(boolean browserState) {
    this.browserState = browserState;

    return this;
  }

  /**
   * Checks if the snapshot includes hidden components.
   *
   * @return true if the snapshot includes hidden components
   */
  public boolean isHiddenComponents() {
    return hiddenComponents;
  }

  /**
   * Sets whether the snapshot includes hidden components. Without it, hidden components and their
   * children are skipped. With it, hidden components are included with the {@code hidden} flag.
   *
   * @param hiddenComponents true to include hidden components
   * @return the options itself
   */
  public SnapshotOptions setHiddenComponents(boolean hiddenComponents) {
    this.hiddenComponents = hiddenComponents;

    return this;
  }

  /**
   * Checks if the snapshot names the component a composite binds.
   *
   * @return true if the snapshot names the component a composite binds
   */
  public boolean isComposites() {
    return composites;
  }

  /**
   * Sets whether the snapshot names the component a composite binds. With it, a composite line
   * carries {@code composite} with the class it binds and {@code node} with the element its chain
   * ends in. Without it, a line names the component alone.
   *
   * @param composites true to name the component a composite binds
   * @return the options itself
   */
  public SnapshotOptions setComposites(boolean composites) {
    this.composites = composites;

    return this;
  }
}
