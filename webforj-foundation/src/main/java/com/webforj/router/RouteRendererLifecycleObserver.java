package com.webforj.router;

import com.webforj.PendingResult;
import com.webforj.component.Component;

/**
 * An interface for observing lifecycle events of components managed by the RouteRenderer.
 *
 * <p>
 * This interface defines lifecycle events specific to the routing and rendering process, such as
 * before and after creating or destroying a component.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
@FunctionalInterface
public interface RouteRendererLifecycleObserver {

  /**
   * Represents the lifecycle events in the routing and rendering process.
   */
  enum LifecycleEvent {
    /**
     * Before the component is created.
     */
    BEFORE_CREATE,

    /**
     * After the component has been created.
     */
    AFTER_CREATE,

    /**
     * Before the component is destroyed.
     */
    BEFORE_DESTROY,

    /**
     * After the component has been destroyed.
     */
    AFTER_DESTROY
  }

  /**
   * Called when a lifecycle event occurs in the routing and rendering process.
   *
   * @param component The component associated with the lifecycle event
   * @param event The type of lifecycle event
   *
   * @return A PendingResult indicating whether the operation should proceed
   */
  PendingResult<Boolean> onRouteRendererLifecycleEvent(Component component, LifecycleEvent event);
}
