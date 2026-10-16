package com.webforj.router.observer;

import com.webforj.component.Component;
import com.webforj.router.NavigationContext;
import com.webforj.router.RouteRenderer;
import com.webforj.router.annotation.RouteRendererObserverPriority;
import java.util.function.Consumer;

/**
 * An interface for observing lifecycle events of components managed by the {@link RouteRenderer}.
 *
 * <p>
 * This interface defines lifecycle events specific to the routing and rendering process, such as
 * before and after creating or destroying a component.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 *
 * @see RouteRenderer
 */
@FunctionalInterface
public interface RouteRendererObserver {

  /**
   * Represents the lifecycle events in the routing and rendering process.
   */
  enum LifecycleEvent {
    /**
     * Before the component is created.
     *
     * <p>
     * The component instance is already constructed when this event fires, and
     * {@link RouteRendererObserver#onInstantiate} is the call that runs before construction.
     * </p>
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
    AFTER_DESTROY,

    /**
     * When a cached component is activated (reused).
     */
    ACTIVATE
  }

  /**
   * Called when a lifecycle event occurs in the routing and rendering process.
   *
   * @param component The component associated with the lifecycle event
   * @param event The type of lifecycle event
   * @param context The navigation context
   * @param continueCallback The callback to indicate whether the operation should proceed. The
   *        consumer should accept {@code true} to continue or {@code false} to halt.
   */
  void onRouteRendererLifecycleEvent(Component component, LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> continueCallback);

  /**
   * Called before the renderer instantiates a route component.
   *
   * <p>
   * This is the only observer call made before the route constructor runs, and halting here means
   * the route is never constructed. The default implementation allows the instantiation.
   * </p>
   *
   * @param componentClass The class of the component about to be instantiated
   * @param context The navigation context
   * @param continueCallback The callback to indicate whether the instantiation should proceed. The
   *        consumer should accept {@code true} to continue or {@code false} to halt.
   * @since 26.03
   */
  default void onInstantiate(Class<? extends Component> componentClass, NavigationContext context,
      Consumer<Boolean> continueCallback) {
    continueCallback.accept(true);
  }

  /**
   * Gets the priority of this observer.
   *
   * <p>
   * Lower values indicate higher priority (executed first). The default implementation returns 10.
   * This method can be overridden to provide custom priority, or the
   * {@link RouteRendererObserverPriority} annotation can be used.
   * </p>
   *
   * @return the priority value, default is 10
   * @since 25.10
   */
  default int getPriority() {
    return 10;
  }
}
