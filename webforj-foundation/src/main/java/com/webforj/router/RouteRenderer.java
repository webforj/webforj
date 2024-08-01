package com.webforj.router;

import com.webforj.App;
import com.webforj.PendingResult;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import com.webforj.data.tree.Vnode;
import com.webforj.data.tree.VnodeDiff;
import com.webforj.router.exception.RouteHasNoTargetException;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.exception.RouteRenderException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The {@code RouteRenderer} class manages the navigation and rendering of components within an
 * application based on predefined routes. It leverages a route registry to resolve the appropriate
 * components and handles their lifecycle, including creation and destruction, according to the
 * current route path.
 *
 * <p>
 * The primary responsibilities of this class include:
 * <ul>
 * <li>Determining the set of components that need to be added or removed based on the route
 * path.</li>
 * <li>Managing component lifecycle events, including handling potential vetoes from lifecycle
 * observers.</li>
 * <li>Ensuring that components are only created or destroyed when appropriate, as dictated by the
 * application's state and observer responses.</li>
 * </ul>
 * </p>
 *
 * <p>
 * Components are maintained in a cache to optimize their lifecycle management. However, the class
 * respects decisions made by lifecycle observers, such that components will not be created or
 * destroyed if observers veto these actions.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRenderer {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> componentsCache = new HashMap<>();
  private final Map<String, Frame> frameCache = new HashMap<>();
  private final List<RouteRendererLifecycleObserver> observers = new ArrayList<>();
  private Vnode<Class<? extends Component>> lastPath;

  /**
   * Constructs a new {@code RouteRenderer} with the specified {@code RouteRegistry}.
   *
   * @param registry the route registry used to determine component routes and targets.
   */
  public RouteRenderer(RouteRegistry registry) {
    this.registry = registry;
  }

  /**
   * Retrieves the current route registry.
   *
   * @return the route registry.
   */
  public RouteRegistry getRegistry() {
    return registry;
  }

  /**
   * Adds a lifecycle observer to the renderer. Observers can veto the creation or destruction of
   * components, thereby influencing the lifecycle management.
   *
   * @param observer the observer to be added.
   */
  public void addLifecycleObserver(RouteRendererLifecycleObserver observer) {
    observers.add(observer);
  }

  /**
   * Removes a previously added lifecycle observer from the renderer.
   *
   * @param observer the observer to be removed.
   */
  public void removeLifecycleObserver(RouteRendererLifecycleObserver observer) {
    observers.remove(observer);
  }

  /**
   * Navigates to the specified component class, triggering the appropriate lifecycle events and
   * managing the addition or removal of components as necessary.
   *
   * <p>
   * This method calculates the difference between the current path and the target path, determining
   * which components need to be added or removed. It then processes removals first, ensuring a
   * clean state before adding new components.
   * </p>
   *
   * <p>
   * The method returns a {@link PendingResult} that completes when the navigation process finishes,
   * either successfully or unsuccessfully. If successful, the result contains the final component
   * that matches the navigation target.
   * </p>
   *
   * @param componentClass the target component class for navigation.
   *
   * @return a pending result with the final component if navigation is successful.
   * @throws RouteNotFoundException if the target route cannot be resolved.
   */
  public PendingResult<Optional<Component>> navigate(Class<? extends Component> componentClass) {
    if (componentClass == null) {
      throw new RouteNotFoundException("Route not found for component: null");
    }

    Optional<Vnode<Class<? extends Component>>> currentPath =
        registry.getComponentsTree(componentClass);
    if (!currentPath.isPresent()) {
      throw new RouteNotFoundException("No route found for component: " + componentClass.getName());
    }

    VnodeDiff<Class<? extends Component>> diff = new VnodeDiff<>(lastPath, currentPath.get());
    Set<Class<? extends Component>> toAdd = diff.getToAdd();
    Set<Class<? extends Component>> toRemove = diff.getToRemove();
    PendingResult<Optional<Component>> result = new PendingResult<>();

    processAllRemoval(toRemove).thenCompose(removalSuccess -> {
      if (Boolean.TRUE.equals(removalSuccess)) {
        return processAllAdditions(toAdd);
      } else {
        return PendingResult.completedWith(false);
      }
    }).thenAccept(additionSuccess -> {
      if (Boolean.TRUE.equals(additionSuccess)) {
        lastPath = currentPath.get();
        result.complete(Optional.ofNullable(componentsCache.get(componentClass)));
      } else {
        result.complete(Optional.empty());
      }
    });

    return result;
  }

  /**
   * Processes the removal of multiple components in the order specified by the path differences.
   *
   * <p>
   * The method handles the removal of components by first notifying the lifecycle observers. If any
   * observer vetoes the removal (returns {@code false}), the component is retained in the cache and
   * the removal process is aborted for that component and all subsequent components.
   * </p>
   *
   * @param toRemove the set of components to be removed.
   * @return a {@link PendingResult} that indicates whether all removals were successful.
   */
  protected PendingResult<Boolean> processAllRemoval(Set<Class<? extends Component>> toRemove) {
    PendingResult<Boolean> result = PendingResult.completedWith(true);

    for (Class<? extends Component> componentClass : toRemove) {
      result = result.thenCompose(success -> {
        if (Boolean.FALSE.equals(success)) {
          return PendingResult.completedWith(false);
        }

        return processSingleRemoval(componentClass);
      });
    }

    return result;
  }

  /**
   * Processes the removal of a single component, ensuring lifecycle observers are notified before
   * the component is removed.
   *
   * <p>
   * If any lifecycle observer denies the removal (vetoes the destruction), the component will not
   * be removed from the cache, ensuring it remains available for use.
   * </p>
   *
   * @param componentClass the component class to be removed.
   * @return a {@link PendingResult} indicating whether the removal was successful.
   */
  protected PendingResult<Boolean> processSingleRemoval(Class<? extends Component> componentClass) {
    Component componentInstance = componentsCache.get(componentClass);
    if (componentInstance == null) {
      return PendingResult.completedWith(true);
    }

    return notify(componentInstance, RouteRendererLifecycleObserver.LifecycleEvent.BEFORE_DESTROY)
        .thenCompose(allowed -> {
          if (Boolean.FALSE.equals(allowed)) {
            // Component destruction is denied; keep it in the cache.
            return PendingResult.completedWith(false);
          }

          detachNode(componentClass, componentInstance);
          return notify(componentInstance,
              RouteRendererLifecycleObserver.LifecycleEvent.AFTER_DESTROY)
                  .thenCompose(success -> PendingResult.completedWith(true));
        });
  }

  /**
   * Detaches a component from its parent, removing it from the visual and logical structure.
   *
   * <p>
   * This method is called after a component has been approved for removal by the lifecycle
   * observers. It handles the actual disconnection of the component from its parent container and
   * ensures that it is removed from the cache to prevent further use.
   * </p>
   *
   * @param componentClass the class of the component to detach.
   * @param componentInstance the instance of the component being detached.
   */
  protected void detachNode(Class<? extends Component> componentClass,
      Component componentInstance) {
    Class<? extends Component> targetClass = registry.getTarget(componentClass);
    Component targetInstance =
        Frame.class.isAssignableFrom(targetClass) ? getFrameComponent(componentClass)
            : getOrCreateComponent(targetClass);

    if (targetInstance instanceof RouteTarget) {
      ((RouteTarget) targetInstance).removeRouteContent(componentInstance);
    } else if (targetInstance instanceof HasComponents) {
      ((HasComponents) targetInstance).remove(componentInstance);
    } else {
      throw new RouteRenderException("Cannot remove route's component in parent "
          + "that does not implement RouteTarget or HasComponents interface. "
          + "Trying to remove component '" + componentInstance.getClass().getName() + "' "
          + "in parent: '" + targetInstance.getClass().getName() + "'");
    }

    componentsCache.remove(componentClass);
  }

  /**
   * Processes the addition of multiple components in the order specified by the path differences.
   *
   * <p>
   * Components are only added if the lifecycle observers approve their creation. If a component is
   * vetoed during the creation process, it is removed from the cache and the process stops.
   * </p>
   *
   * @param toAdd the set of components to be added.
   * @return a {@link PendingResult} that indicates whether all additions were successful.
   */
  protected PendingResult<Boolean> processAllAdditions(Set<Class<? extends Component>> toAdd) {
    PendingResult<Boolean> result = PendingResult.completedWith(true);

    for (Class<? extends Component> componentClass : toAdd) {
      // frames are rendered by the application
      if (Frame.class.isAssignableFrom(componentClass)) {
        continue;
      }

      result = result.thenCompose(success -> {
        if (Boolean.FALSE.equals(success)) {
          return PendingResult.completedWith(false);
        }

        return processSingleAddition(componentClass);
      });
    }

    return result;
  }

  /**
   * Processes the addition of a single component, ensuring lifecycle observers are notified before
   * the component is added.
   *
   * <p>
   * This method first checks with lifecycle observers to determine if the component creation is
   * allowed. If any observer vetoes the creation (returns {@code false}), the component is removed
   * from the cache.
   * </p>
   *
   * @param componentClass the component class to be added.
   * @return a {@link PendingResult} indicating whether the addition was successful.
   */
  protected PendingResult<Boolean> processSingleAddition(
      Class<? extends Component> componentClass) {
    return getOrCreateComponentAsync(componentClass).thenCompose(componentInstance -> {
      if (componentInstance == null) {
        componentsCache.remove(componentClass);
        return PendingResult.completedWith(false);
      }

      return notify(componentInstance, RouteRendererLifecycleObserver.LifecycleEvent.BEFORE_CREATE)
          .thenCompose(allowed -> {
            if (Boolean.FALSE.equals(allowed)) {
              componentsCache.remove(componentClass);
              return PendingResult.completedWith(false);
            }

            attachNode(componentClass, componentInstance);
            return notify(componentInstance,
                RouteRendererLifecycleObserver.LifecycleEvent.AFTER_CREATE)
                    .thenCompose(success -> PendingResult.completedWith(true));
          });
    });
  }

  /**
   * Attaches a component to its parent, integrating it into the visual and logical structure.
   *
   * <p>
   * This method is called after a component has been approved for creation by the lifecycle
   * observers. It handles the actual connection of the component to its parent container and
   * ensures it is added to the cache for subsequent use.
   * </p>
   *
   * @param componentClass the class of the component to attach.
   * @param componentInstance the instance of the component being attached.
   */
  protected void attachNode(Class<? extends Component> componentClass,
      Component componentInstance) {
    Class<? extends Component> targetClass = registry.getTarget(componentClass);
    if (targetClass == null) {
      throw new RouteHasNoTargetException(
          "No route target found for component: " + componentClass.getName()
              + ", route is registered as " + registry.getRouteByComponent(componentClass)
              + " If no target is required, use Frame.class as the target.");
    }

    Component targetInstance =
        Frame.class.isAssignableFrom(targetClass) ? getFrameComponent(componentClass)
            : getOrCreateComponent(targetClass);

    if (targetInstance instanceof RouteTarget) {
      ((RouteTarget) targetInstance).showRouteContent(componentInstance);
    } else if (targetInstance instanceof HasComponents) {
      ((HasComponents) targetInstance).add(componentInstance);
    } else {
      throw new RouteRenderException("Cannot render route's component in parent "
          + "that does not implement RouteTarget or HasComponents interface. "
          + "Trying to render component '" + componentInstance.getClass().getName() + "' "
          + "in parent: '" + targetInstance.getClass().getName() + "'");
    }
  }

  /**
   * Retrieves or creates a component based on its class. This method uses the cache to optimize
   * component reuse, creating a new instance only if necessary.
   *
   * @param componentClass the class of the component to retrieve or create.
   * @return the component instance, either from the cache or newly created.
   */
  private Component getOrCreateComponent(Class<? extends Component> componentClass) {
    // Use the cache in case the component has already been created
    Component componentInstance = componentsCache.get(componentClass);
    if (componentInstance == null || componentInstance.isDestroyed()) {
      // try to create the component instance
      try {
        componentInstance = componentClass.getDeclaredConstructor().newInstance();
        componentsCache.put(componentClass, componentInstance);

        componentInstance.addLifecycleObserver((component, event) -> {
          if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
            componentsCache.remove(componentClass);
          }
        });
      } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
          | InvocationTargetException | NoSuchMethodException | SecurityException e) {
        throw new RouteRenderException("Failed to render component instance: " + componentClass, e);
      }
    }

    return componentInstance;
  }

  /**
   * Asynchronously retrieves or creates a component based on its class. This method leverages the
   * lifecycle observers to determine if the component should be created.
   *
   * <p>
   * If the component creation is vetoed, it is removed from the cache to prevent accidental reuse.
   * The method ensures that only valid components are present in the cache.
   * </p>
   *
   * @param componentClass the class of the component to retrieve or create.
   * @return a {@link PendingResult} that completes with the component instance or null if denied.
   */
  protected PendingResult<Component> getOrCreateComponentAsync(
      Class<? extends Component> componentClass) {
    Component[] componentHolder = new Component[1];
    componentHolder[0] = componentsCache.get(componentClass);

    if (componentHolder[0] == null || componentHolder[0].isDestroyed()) {
      return notify(null, RouteRendererLifecycleObserver.LifecycleEvent.BEFORE_CREATE)
          .thenCompose(allowed -> {
            if (Boolean.FALSE.equals(allowed)) {
              componentsCache.remove(componentClass);
              return PendingResult.completedWith(null);
            }

            Component newComponent = getOrCreateComponent(componentClass);
            if (newComponent == null) {
              componentsCache.remove(componentClass);
            }

            return PendingResult.completedWith(newComponent);
          });
    }

    return PendingResult.completedWith(componentHolder[0]);
  }

  /**
   * Retrieves a frame component based on the specified component class. Frames are managed
   * differently from regular components and are stored in a separate cache.
   *
   * @param componentClass the class of the component to retrieve the frame for.
   * @return the frame instance associated with the component class.
   */
  @SuppressWarnings("squid:S3776")
  protected Frame getFrameComponent(Class<? extends Component> componentClass) {
    String frameId = registry.getFrameRouteId(componentClass);
    String cacheKey = frameId != null ? frameId : "com.webforj.utilities.WelcomeApp";
    final boolean[] isNewFrame = {false};

    Frame foundFrame = frameCache.computeIfAbsent(cacheKey, id -> {
      for (Frame frame : App.getFrames()) {
        if (frameId != null) {
          if (frame.getFrameId().equals(id)) {
            return frame;
          }
        } else {
          // Use the first frame if no frame ID is provided
          if (!frame.getFrameId().equals(cacheKey)) {
            return frame;
          }
        }
      }

      isNewFrame[0] = true;
      return null;
    });

    if (foundFrame != null && isNewFrame[0]) {
      foundFrame.addLifecycleObserver((component, event) -> {
        if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
          frameCache.remove(cacheKey);
        }
      });
    }

    return foundFrame;
  }

  /**
   * Notifies the lifecycle observers of a given component of the specified event. The event can
   * include lifecycle stages such as creation, destruction, etc.
   *
   * <p>
   * The method sequentially calls each observer, allowing them to approve or veto the lifecycle
   * event. The result indicates whether the event should proceed.
   * </p>
   *
   * @param component the component associated with the event.
   * @param event the lifecycle event.
   *
   * @return a {@link PendingResult} indicating whether the event should proceed.
   */
  protected PendingResult<Boolean> notify(Component component,
      RouteRendererLifecycleObserver.LifecycleEvent event) {
    PendingResult<Boolean> result = PendingResult.completedWith(true);
    for (RouteRendererLifecycleObserver observer : observers) {
      result = result.thenCompose(allowed -> {
        if (Boolean.FALSE.equals(allowed)) {
          return PendingResult.completedWith(false);
        }

        return observer.onRouteRendererLifecycleEvent(component, event);
      });
    }

    return result;
  }
}
