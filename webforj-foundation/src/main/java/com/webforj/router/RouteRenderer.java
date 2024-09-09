package com.webforj.router;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import com.webforj.data.WorkflowExecutor;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.exception.RouteRenderException;
import com.webforj.router.observer.RouteRendererObserver;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

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
 * @since 24.12
 */
public class RouteRenderer {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> componentsCache = new HashMap<>();
  private final Map<String, Frame> frameCache = new HashMap<>();
  private final List<RouteRendererObserver> observers = new ArrayList<>();
  private RouteRelation<Class<? extends Component>> lastPath;

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
  public void addObserver(RouteRendererObserver observer) {
    observers.add(observer);
  }

  /**
   * Removes a previously added lifecycle observer from the renderer.
   *
   * @param observer the observer to be removed.
   */
  public void removeObserver(RouteRendererObserver observer) {
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
   * The method returns an Optional component that matches the navigation target.
   * </p>
   *
   * @param <T> the type of the the component.
   * @param component the target component class for navigation.
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the navigation.
   *
   * @throws RouteNotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, NavigationContext context,
      Consumer<Optional<T>> onComplete) {
    if (component == null) {
      throw new RouteNotFoundException("Route not found for component: null");
    }

    Optional<RouteRelation<Class<? extends Component>>> currentPath =
        registry.getComponentsTree(component);
    if (!currentPath.isPresent()) {
      throw new RouteNotFoundException("No route found for component: " + component.getName());
    }

    RouteRelationDiff<Class<? extends Component>> diff =
        new RouteRelationDiff<>(lastPath, currentPath.get());
    Set<Class<? extends Component>> toAdd = diff.getToAdd();
    Set<Class<? extends Component>> toRemove = diff.getToRemove();

    processRemovals(toRemove, context, removalSuccess -> {
      if (Boolean.TRUE.equals(removalSuccess)) {
        processAdditions(toAdd, context, additionSuccess -> {
          if (Boolean.TRUE.equals(additionSuccess)) {
            lastPath = currentPath.get();
          }

          if (onComplete != null) {
            Component componentCache = componentsCache.get(component);
            T componentInstance = componentCache != null ? component.cast(componentCache) : null;
            onComplete.accept(
                Boolean.TRUE.equals(additionSuccess) ? Optional.ofNullable(componentInstance)
                    : Optional.empty());
          }
        });
      } else {
        if (onComplete != null) {
          onComplete.accept(Optional.empty());
        }
      }
    });
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
   * @param <T> the type of the the component.
   * @param component the target component class for navigation.
   * @param context the navigation context to pass through the process.
   *
   * @throws RouteNotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, NavigationContext context) {
    render(component, context, null);
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
   * The method returns an Optional component that matches the navigation target.
   * </p>
   *
   * @param <T> the type of the the component.
   * @param component the target component class for navigation.
   * @param onComplete the callback to be invoked with the result of the navigation.
   *
   * @throws RouteNotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, Consumer<Optional<T>> onComplete) {
    render(component, null, onComplete);
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
   * @param <T> the type of the the component.
   * @param component the target component class for navigation.
   *
   * @throws RouteNotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component) {
    render(component, null, null);
  }

  /**
   * Processes the removal of multiple components, ensuring lifecycle observers are notified before
   * each component is removed. The process is halted if any observer vetoes a removal.
   *
   * @param componentsToRemove the components to be removed.
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processRemovals(Set<Class<? extends Component>> componentsToRemove,
      NavigationContext context, Consumer<Boolean> onComplete) {
    List<Class<? extends Component>> componentList = new ArrayList<>(componentsToRemove);
    // reverse the list to remove the leaf nodes first
    Collections.reverse(componentList);
    WorkflowExecutor<Boolean> executor = new WorkflowExecutor<>();
    AtomicBoolean removalFailed = new AtomicBoolean(false);

    for (Class<? extends Component> componentClass : componentList) {
      executor.addTask((ctx, cb) -> {
        if (removalFailed.get()) {
          cb.accept(false); // Skip remaining removals if any have failed
          return;
        }
        processSingleRemoval(componentClass, context, success -> {
          if (!Boolean.TRUE.equals(success)) {
            removalFailed.set(true);
          }
          cb.accept(success);
        });
      });
    }

    executor.run(null, success -> onComplete.accept(!removalFailed.get()));
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
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processSingleRemoval(Class<? extends Component> componentClass,
      NavigationContext context, Consumer<Boolean> onComplete) {
    if (Frame.class.isAssignableFrom(componentClass)) {
      onComplete.accept(true);
      return;
    }

    Component componentInstance = componentsCache.get(componentClass);
    if (componentInstance == null
        || (componentInstance != null && componentInstance.isDestroyed())) {
      onComplete.accept(true);
      return;
    }

    notify(componentInstance, RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY, context,
        allowed -> {
          if (Boolean.FALSE.equals(allowed)) {
            onComplete.accept(false);
            return;
          }

          detachNode(componentClass, componentInstance);
          notify(componentInstance, RouteRendererObserver.LifecycleEvent.AFTER_DESTROY, context,
              success -> onComplete.accept(true));
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
    Optional<Class<? extends Component>> outletClass = registry.getOutlet(componentClass);
    if (!outletClass.isPresent()) {
      outletClass = Optional.ofNullable(Frame.class);
    }

    if (componentInstance.isDestroyed()) {
      componentsCache.remove(componentClass);
      return;
    }

    Component outletInstance =
        Frame.class.isAssignableFrom(outletClass.get()) ? getFrameComponent(componentClass)
            : getOrCreateComponent(outletClass.get());

    if (outletInstance instanceof RouteOutlet) {
      ((RouteOutlet) outletInstance).removeRouteContent(componentInstance);
    } else if (outletInstance instanceof HasComponents) {
      ((HasComponents) outletInstance).remove(componentInstance);
    } else {
      throw new RouteRenderException("Cannot remove route's component in outlet "
          + "that does not implement RouteOutlet or HasComponents interface. "
          + "Trying to remove component '" + componentInstance.getClass().getName() + "' "
          + "in outlet: '" + outletInstance.getClass().getName() + "'");
    }

    componentsCache.remove(componentClass);
  }

  /**
   * Processes the addition of multiple components, ensuring lifecycle observers are notified before
   * each component is added. The process is halted if any observer vetoes an addition.
   *
   * @param componentsToAdd the components to be added.
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processAdditions(Set<Class<? extends Component>> componentsToAdd,
      NavigationContext context, Consumer<Boolean> onComplete) {
    List<Class<? extends Component>> componentList = new ArrayList<>(componentsToAdd);
    WorkflowExecutor<Boolean> executor = new WorkflowExecutor<>();
    AtomicBoolean additionFailed = new AtomicBoolean(false);

    for (Class<? extends Component> componentClass : componentList) {
      executor.addTask((ctx, cb) -> {
        if (additionFailed.get()) {
          cb.accept(false); // Skip remaining additions if any have failed
          return;
        }
        processSingleAddition(componentClass, context, success -> {
          if (!Boolean.TRUE.equals(success)) {
            additionFailed.set(true);
          }
          cb.accept(success);
        });
      });
    }

    executor.run(null, success -> onComplete.accept(!additionFailed.get()));
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
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processSingleAddition(Class<? extends Component> componentClass,
      NavigationContext context, Consumer<Boolean> onComplete) {
    if (Frame.class.isAssignableFrom(componentClass)) {
      onComplete.accept(true);
      return;
    }

    getOrCreateComponentAsync(componentClass, context, componentInstance -> {
      if (componentInstance == null) {
        componentsCache.remove(componentClass);
        onComplete.accept(false);
        return;
      }

      if (componentInstance.isAttached()) {
        onComplete.accept(true);
        return;
      }

      notify(componentInstance, RouteRendererObserver.LifecycleEvent.BEFORE_CREATE, context,
          allowed -> {
            if (Boolean.FALSE.equals(allowed)) {
              componentsCache.remove(componentClass);
              onComplete.accept(false);
              return;
            }

            attachNode(componentClass, componentInstance);
            notify(componentInstance, RouteRendererObserver.LifecycleEvent.AFTER_CREATE, context,
                success -> onComplete.accept(true));
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

    if (componentInstance.isAttached()) {
      return;
    }

    Optional<Class<? extends Component>> outletClass = registry.getOutlet(componentClass);
    if (!outletClass.isPresent()) {
      outletClass = Optional.ofNullable(Frame.class);
    }

    Component outletInstance =
        Frame.class.isAssignableFrom(outletClass.get()) ? getFrameComponent(componentClass)
            : getOrCreateComponent(outletClass.get());

    if (outletInstance instanceof RouteOutlet) {
      ((RouteOutlet) outletInstance).showRouteContent(componentInstance);
    } else if (outletInstance instanceof HasComponents) {
      ((HasComponents) outletInstance).add(componentInstance);
    } else {
      throw new RouteRenderException("Cannot render route's component in outlet "
          + "that does not implement RouteOutlet or HasComponents interface. "
          + "Trying to render component '" + componentInstance.getClass().getName() + "' "
          + "in outlet: '" + outletInstance.getClass().getName() + "'");
    }
  }

  /**
   * Retrieves or creates a component based on its class. This method uses the cache to optimize
   * component reuse, creating a new instance only if necessary.
   *
   * @param componentClass the class of the component to retrieve or create.
   * @return the component instance, either from the cache or newly created.
   */
  protected Component getOrCreateComponent(Class<? extends Component> componentClass) {
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
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void getOrCreateComponentAsync(Class<? extends Component> componentClass,
      NavigationContext context, Consumer<Component> onComplete) {
    Component componentInstance = componentsCache.get(componentClass);

    if (componentInstance == null || componentInstance.isDestroyed()) {
      notify(null, RouteRendererObserver.LifecycleEvent.BEFORE_CREATE, context, allowed -> {
        if (Boolean.FALSE.equals(allowed)) {
          componentsCache.remove(componentClass);
          onComplete.accept(null);
          return;
        }

        Component newComponent = getOrCreateComponent(componentClass);
        if (newComponent == null) {
          componentsCache.remove(componentClass);
        }

        onComplete.accept(newComponent);
      });
    } else {
      onComplete.accept(componentInstance);
    }
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
    String frameId = registry.getFrameRouteId(componentClass).orElse(null);
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
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the notification.
   */
  protected void notify(Component component, RouteRendererObserver.LifecycleEvent event,
      NavigationContext context, Consumer<Boolean> onComplete) {
    WorkflowExecutor<Boolean> executor = new WorkflowExecutor<>();
    AtomicBoolean vetoed = new AtomicBoolean(false);

    for (RouteRendererObserver observer : observers) {
      executor.addTask((ctx, cb) -> {
        if (vetoed.get()) {
          cb.accept(false); // Skip further notification if vetoed
          return;
        }
        observer.onRouteRendererLifecycleEvent(component, event, context, result -> {
          if (Boolean.FALSE.equals(result)) {
            vetoed.set(true);
            cb.accept(false);
            onComplete.accept(false); // Stop workflow if vetoed
            return;
          }
          cb.accept(true);
        });
      });
    }

    executor.run(null, success -> {
      if (!vetoed.get()) {
        onComplete.accept(true);
      }
    });
  }
}
