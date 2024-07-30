package com.webforj.router;

import com.webforj.App;
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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * The {@code RouteRenderer} class is responsible for navigating and rendering components based on
 * registered routes.
 *
 * <p>
 * This class is class is responsible for navigating and rendering components based on registered
 * routes. The class uses the {@code RouteRegistry} to resolve the routes and render the components
 * in the parent component according to the route configuration. The navigator keeps track of the
 * last path that was rendered and only renders the components that have changed since the last
 * render.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRenderer {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> componentsCache = new HashMap<>();
  private final Map<String, Frame> frameCache = new HashMap<>();
  private Vnode<Class<? extends Component>> lastPath;

  /**
   * Constructs a new {@code RouteRenderer} instance with the given {@code RouteRegistry}.
   *
   * @param registry the route registry
   */
  public RouteRenderer(RouteRegistry registry) {
    this.registry = registry;
  }

  /**
   * Retrieves the route registry.
   *
   * @return the route registry
   */
  public RouteRegistry getRegistry() {
    return registry;
  }

  /**
   * Navigates to the given component class.
   *
   * @param componentClass the component class to render
   * @return the rendered component
   */
  public Component navigate(Class<? extends Component> componentClass) {
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

    detachNodes(toRemove);
    attachNodes(toAdd);

    this.lastPath = currentPath.get();
    return componentsCache.get(componentClass);
  }

  private void attachNodes(Set<Class<? extends Component>> nodes) {
    Iterator<Class<? extends Component>> iterator = nodes.iterator();
    while (iterator.hasNext()) {
      Class<? extends Component> node = iterator.next();

      // If the root node is a Frame, render its children
      // we don't need to render the Frame itself, frames
      // are rendered by the application
      if (Frame.class.isAssignableFrom(node)) {
        continue;
      }

      Component componentInstance = getOrCreateComponent(node);
      Class<? extends Component> targetClass = registry.getTarget(node);
      if (targetClass == null) {
        throw new RouteHasNoTargetException("No route target found for component: " + node.getName()
            + ", route is registered as " + registry.getRouteByComponent(node)
            + " If no target is required, use Frame.class as the target.");
      }

      Component targetInstance = Frame.class.isAssignableFrom(targetClass) ? getFrameComponent(node)
          : getOrCreateComponent(targetClass);

      if (targetInstance instanceof RouteTarget) {
        ((RouteTarget) targetInstance).showRouteContent(componentInstance);
      } else if (targetInstance instanceof HasComponents) {
        ((HasComponents) targetInstance).add(componentInstance);
      } else {
        throw new RouteRenderException("Cannot render route's component in parent "
            + "that does not implement RouteTarget or HasComponents interface. "
            + "Trying to render component '" + componentInstance.getClass().getName()
            + "' in parent: " + targetInstance.getClass().getName());
      }
    }
  }

  private void detachNodes(Set<Class<? extends Component>> node) {
    Iterator<Class<? extends Component>> iterator = node.iterator();
    while (iterator.hasNext()) {
      Class<? extends Component> nodeClass = iterator.next();
      Component componentInstance = componentsCache.get(nodeClass);
      if (componentInstance == null) {
        continue;
      }

      Class<? extends Component> targetClass = registry.getTarget(nodeClass);
      Component targetInstance =
          Frame.class.isAssignableFrom(targetClass) ? getFrameComponent(nodeClass)
              : getOrCreateComponent(targetClass);

      if (targetInstance instanceof RouteTarget) {
        ((RouteTarget) targetInstance).removeRouteContent(componentInstance);
      } else if (targetInstance instanceof HasComponents) {
        ((HasComponents) targetInstance).remove(componentInstance);
      } else {
        throw new RouteRenderException("Cannot remove route's component in parent "
            + "that does not implement RouteTarget or HasComponents interface. "
            + "Trying to remove component '" + componentInstance.getClass().getName()
            + "' in parent: " + targetInstance.getClass().getName());
      }
    }
  }

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

  private Frame getFrameComponent(Class<? extends Component> componentClass) {
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
}
