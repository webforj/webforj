package com.webforj.component;

/**
 * An interface for observing lifecycle events of a component.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
@FunctionalInterface
public interface ComponentLifecycleObserver {
  /**
   * Represents the lifecycle events of a component.
   */
  enum LifecycleEvent {
    /**
     * Indicates that the component has been created and attached.
     */
    CREATE,

    /**
     * Indicates that the component has been destroyed.
     */
    DESTROY
  }

  /**
   * Called when a lifecycle event occurs for the observed component.
   *
   * @param component The component that triggered the event
   * @param event The type of lifecycle event
   */
  void onComponentLifecycleEvent(Component component, LifecycleEvent event);
}
