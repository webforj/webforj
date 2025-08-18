package com.webforj.router;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.ComponentUtil;
import com.webforj.component.Composite;
import com.webforj.component.window.Frame;
import com.webforj.conceiver.Conceiver;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.concern.HasComponents;
import com.webforj.data.WorkflowExecutor;
import com.webforj.router.exception.NotFoundException;
import com.webforj.router.exception.RouteRenderException;
import com.webforj.router.observer.RouteRendererObserver;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
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
 */
public class RouteRenderer {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> componentsCache = new HashMap<>();
  private final Map<String, Frame> frameCache = new HashMap<>();
  private final List<RouteRendererObserver> observers = new ArrayList<>();
  private RouteRelation<Class<? extends Component>> lastPath;
  private RouteRelation<Class<? extends Component>> currentlyRenderingPath;
  private NavigationContext context;

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
   * @param <T> the type of the component.
   * @param component the target component class for navigation.
   * @param context the navigation context to pass through the process.
   * @param onComplete the callback to be invoked with the result of the navigation.
   *
   * @throws NotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, NavigationContext context,
      Consumer<Optional<T>> onComplete) {
    if (component == null) {
      throw new NotFoundException("Route not found for component: null");
    }

    Optional<RouteRelation<Class<? extends Component>>> currentPath =
        registry.getComponentHierarchy(component);
    if (!currentPath.isPresent()) {
      throw new NotFoundException("No route found for component: " + component.getName());
    }

    this.context = context;
    // Set the path being rendered BEFORE creating components
    this.currentlyRenderingPath = currentPath.get();

    RouteRelationDiff<Class<? extends Component>> diff =
        new RouteRelationDiff<>(lastPath, currentPath.get());
    Set<Class<? extends Component>> toAdd = new LinkedHashSet<>(diff.getToAdd());
    Set<Class<? extends Component>> toRemove = diff.getToRemove();

    // Add cached components that need activation
    for (RouteRelation<Class<? extends Component>> node : currentPath.get()) {
      Class<? extends Component> pathComponent = node.getData();
      if (!toRemove.contains(pathComponent) && componentsCache.containsKey(pathComponent)) {
        toAdd.add(pathComponent);
      }
    }

    processRemovals(toRemove, removalSuccess -> {
      if (Boolean.TRUE.equals(removalSuccess)) {
        processAdditions(toAdd, additionSuccess -> {
          if (Boolean.TRUE.equals(additionSuccess)) {
            lastPath = currentPath.get();
          }
          // Clear the currently rendering path after completion
          this.currentlyRenderingPath = null;

          if (onComplete != null) {
            Component componentCache = componentsCache.get(component);
            T componentInstance = componentCache != null ? component.cast(componentCache) : null;
            onComplete.accept(
                Boolean.TRUE.equals(additionSuccess) ? Optional.ofNullable(componentInstance)
                    : Optional.empty());
          }
        });
      } else {
        // Clear on failure too
        this.currentlyRenderingPath = null;
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
   * @param <T> the type of the component.
   * @param component the target component class for navigation.
   * @param context the navigation context to pass through the process.
   *
   * @throws NotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, NavigationContext context) {
    render(component, context, null);
  }

  /**
   * Navigates to the specified component class, triggering the appropriate lifecycle events and
   * managing the addition or removal of components as necessary.
   *
   * <p>
   * The method returns an Optional component that matches the navigation target.
   * </p>
   *
   * @param <T> the type of the component.
   * @param component the target component class for navigation.
   * @param onComplete the callback to be invoked with the result of the navigation.
   *
   * @throws NotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component, Consumer<Optional<T>> onComplete) {
    render(component, new NavigationContext(), onComplete);
  }

  /**
   * Navigates to the specified component class, triggering the appropriate lifecycle events and
   * managing the addition or removal of components as necessary.
   *
   * @param <T> the type of the component.
   * @param component the target component class for navigation.
   *
   * @throws NotFoundException if the target route cannot be resolved.
   */
  public <T extends Component> void render(Class<T> component) {
    render(component, (c) -> {
      // no-op
    });
  }

  /**
   * Retrieves the rendered component instance for the specified component class. This method
   * returns the component instance from the cache, if available.
   *
   * @param component the component class to retrieve.
   * @return an optional containing the component instance, if available.
   */
  public Optional<Component> getRenderedComponent(Class<? extends Component> component) {
    return Optional.ofNullable(componentsCache.get(component));
  }

  /**
   * Retrieves the currently active route hierarchy.
   *
   * <p>
   * This method returns the route hierarchy that is currently active. This includes both the
   * hierarchy being rendered (if any) and the last successfully rendered hierarchy. This is the
   * authoritative source for determining which route is currently active in the application.
   * </p>
   *
   * @return an optional containing the active route hierarchy, or empty if no route has been
   *         rendered
   * @since 25.03
   */
  public Optional<RouteRelation<Class<? extends Component>>> getActiveRoutePath() {
    // If currently rendering, return the path being rendered
    if (currentlyRenderingPath != null) {
      return Optional.of(currentlyRenderingPath);
    }
    // Otherwise return the last successfully rendered path
    return Optional.ofNullable(lastPath);
  }

  /**
   * Processes the removal of multiple components, ensuring lifecycle observers are notified before
   * each component is removed. The process is halted if any observer vetoes a removal.
   *
   * @param componentsToRemove the components to be removed.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processRemovals(Set<Class<? extends Component>> componentsToRemove,
      Consumer<Boolean> onComplete) {
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

        processSingleRemoval(componentClass, success -> {
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
   * @param componentClass the component class to be removed.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processSingleRemoval(Class<? extends Component> componentClass,
      Consumer<Boolean> onComplete) {
    if (Frame.class.isAssignableFrom(componentClass)) {
      onComplete.accept(true);
      return;
    }

    Component componentInstance = componentsCache.get(componentClass);
    if (componentInstance == null || componentInstance.isDestroyed()) {
      componentsCache.remove(componentClass);
      onComplete.accept(true);
      return;
    }

    notify(componentInstance, RouteRendererObserver.LifecycleEvent.BEFORE_DESTROY, allowed -> {
      if (Boolean.FALSE.equals(allowed)) {
        onComplete.accept(false);
        return;
      }

      detachNode(componentClass, componentInstance);
      notify(componentInstance, RouteRendererObserver.LifecycleEvent.AFTER_DESTROY,
          success -> onComplete.accept(true));
    });
  }

  /**
   * Detaches a component from its parent, removing it from the visual and logical structure.
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

    boolean isFrame = Frame.class.isAssignableFrom(outletClass.get());
    Component outletInstance =
        isFrame ? getFrameComponent(componentClass) : componentsCache.get(outletClass.get());

    if (isFrame) {
      context.setActiveFrame((Frame) outletInstance);
    }

    removeComponentFromParent(componentInstance, outletInstance);
  }

  /**
   * Processes the addition of multiple components, ensuring lifecycle observers are notified before
   * each component is added. The process is halted if any observer vetoes an addition.
   *
   * @param componentsToAdd the components to be added.
   * @param onComplete the callback to be invoked with the result of the operation.
   */
  protected void processAdditions(Set<Class<? extends Component>> componentsToAdd,
      Consumer<Boolean> onComplete) {
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

    getOrCreateComponentWithLifecycle(componentClass, success -> {
      if (!Boolean.TRUE.equals(success)) {
        onComplete.accept(false);
        return;
      }

      Component componentInstance = componentsCache.get(componentClass);
      if (!componentInstance.isAttached()) {
        attachNode(componentClass, componentInstance, attachSuccess -> {
          onComplete.accept(attachSuccess);
        });
      } else {
        onComplete.accept(true);
      }
    });
  }

  /**
   * Attaches a component to its parent, integrating it into the visual and logical structure.
   *
   * @param componentClass the class of the component to attach.
   * @param componentInstance the instance of the component being attached.
   */
  protected void attachNode(Class<? extends Component> componentClass, Component componentInstance,
      Consumer<Boolean> onComplete) {

    Optional<Class<? extends Component>> outletClass = registry.getOutlet(componentClass);
    if (!outletClass.isPresent()) {
      outletClass = Optional.of(Frame.class);
    }

    final Class<? extends Component> outletClassFinal = outletClass.get();
    boolean isFrame = Frame.class.isAssignableFrom(outletClassFinal);

    if (isFrame) {
      Component outletInstance = getFrameComponent(componentClass);
      context.setActiveFrame((Frame) outletInstance);
      addComponentToParent(componentInstance, outletInstance);
      onComplete.accept(true);
    } else {
      getOrCreateComponentWithLifecycle(outletClassFinal, success -> {
        if (!Boolean.TRUE.equals(success)) {
          onComplete.accept(false);
          return;
        }
        Component outletInstance = componentsCache.get(outletClassFinal);
        if (outletInstance == null) {
          onComplete.accept(false);
          return;
        }
        addComponentToParent(componentInstance, outletInstance);
        onComplete.accept(true);
      });
    }
  }

  /**
   * Retrieves or creates a component based on its class, ensuring lifecycle events are invoked.
   *
   * @param componentClass the class of the component to retrieve or create.
   * @param onComplete the callback to be invoked with the result.
   */
  protected void getOrCreateComponentWithLifecycle(Class<? extends Component> componentClass,
      Consumer<Boolean> onComplete) {
    if (Frame.class.isAssignableFrom(componentClass)) {
      onComplete.accept(true);
      return;
    }

    Component componentInstance = componentsCache.get(componentClass);

    if (componentInstance == null || componentInstance.isDestroyed()) {
      Component newComponentInstance = getConceiver().getComponent(componentClass);
      notify(newComponentInstance, RouteRendererObserver.LifecycleEvent.BEFORE_CREATE, allowed -> {
        if (Boolean.FALSE.equals(allowed)) {
          onComplete.accept(false);
          return;
        }

        componentsCache.put(componentClass, newComponentInstance);
        context.addComponent(newComponentInstance);

        // Add lifecycle observer to remove from cache when destroyed
        newComponentInstance.addLifecycleObserver((component, event) -> {
          if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
            componentsCache.remove(componentClass);
          }
        });

        notify(newComponentInstance, RouteRendererObserver.LifecycleEvent.AFTER_CREATE,
            afterCreateAllowed -> {
              onComplete.accept(true);
            });
      });
    } else {
      // Component already exists - activate it
      context.addComponent(componentInstance);
      notify(componentInstance, RouteRendererObserver.LifecycleEvent.ACTIVATE, ignored -> {
        onComplete.accept(true);
      });
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
    String frameId = registry.getRouteFrameName(componentClass).orElse(null);
    String cacheKey = frameId != null ? frameId : "com.webforj.utilities.WelcomeApp";
    final boolean[] isNewFrame = {false};

    Frame foundFrame = frameCache.computeIfAbsent(cacheKey, id -> {
      for (Frame frame : App.getFrames()) {
        if (frameId != null) {
          if (frame.getName().equals(id)) {
            return frame;
          }
        } else {
          // Use the first frame if no frame ID is provided
          if (!frame.getName().equals(cacheKey)) {
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
   * Notifies the lifecycle observers of a given component of the specified event.
   *
   * @param component the component associated with the event.
   * @param event the lifecycle event.
   * @param onComplete the callback to be invoked with the result of the notification.
   */
  protected void notify(Component component, RouteRendererObserver.LifecycleEvent event,
      Consumer<Boolean> onComplete) {
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

  private void addComponentToParent(Component componentInstance, Component outletInstance) {
    if (outletInstance instanceof RouteOutlet outlet) {
      outlet.showRouteContent(componentInstance);
    } else if (outletInstance instanceof HasComponents hasComponents) {
      hasComponents.add(componentInstance);
    } else if (outletInstance instanceof Composite) {
      addComponentToParent(componentInstance, ComponentUtil.getBoundComponent(outletInstance));
    } else {
      String componentClassName =
          componentInstance != null ? componentInstance.getClass().getName() : "null";
      String outletClassName =
          outletInstance != null ? outletInstance.getClass().getName() : "null";
      throw new RouteRenderException("Cannot render route's component in outlet "
          + "that does not implement RouteOutlet or HasComponents interface. "
          + "Trying to render component '" + componentClassName + "' " + "in outlet: '"
          + outletClassName + "'");
    }
  }

  private void removeComponentFromParent(Component componentInstance, Component outletInstance) {
    if (outletInstance instanceof RouteOutlet outlet) {
      outlet.removeRouteContent(componentInstance);
    } else if (outletInstance instanceof HasComponents hasComponents) {
      hasComponents.remove(componentInstance);
    } else if (outletInstance instanceof Composite) {
      removeComponentFromParent(componentInstance, ComponentUtil.getBoundComponent(outletInstance));
    } else {
      String componentClassName =
          componentInstance != null ? componentInstance.getClass().getName() : "null";
      String outletClassName =
          outletInstance != null ? outletInstance.getClass().getName() : "null";
      throw new RouteRenderException("Cannot remove route's component in outlet "
          + "that does not implement RouteOutlet or HasComponents interface. "
          + "Trying to remove component '" + componentClassName + "' " + "in outlet: '"
          + outletClassName + "'");
    }
  }

  Conceiver getConceiver() {
    return ConceiverProvider.getCurrent();
  }
}
