package com.webforj.router;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import com.webforj.router.exception.RouteHasNoTargetException;
import com.webforj.router.exception.RouteNotFoundException;
import com.webforj.router.exception.RouteRenderException;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

/**
 * The {@code RouteRenderer} class is responsible for rendering the components for the given routes.
 *
 * <p>
 * This class is responsible for rendering the components for the given routes. The class will
 * create an instance of the component and render it in the parent component. If the parent
 * component is a Frame, the component will be rendered in the Frame.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RouteRenderer {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> viewCache = new HashMap<>();
  private final Map<Object, Component> currentViews = new HashMap<>();
  private final Map<String, Frame> frameCache = new HashMap<>();

  /**
   * Constructs a new {@code RouteResolver} instance with the given {@code RouteRegistry}.
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
   * Renders the component for the given route.
   *
   * <p>
   * This method renders the component for the given route. The method will create an instance of
   * the component and render it in the parent component. If the parent component is a Frame, the
   * component will be rendered in the Frame.
   * </p>
   *
   * @param componentClass the component class to render
   * @return the rendered component
   */
  public Component render(Class<? extends Component> componentClass) {
    if (componentClass == null) {
      throw new RouteNotFoundException("Route not found for component: null");
    }

    Class<? extends Component> targetClass = registry.getTarget(componentClass);
    if (targetClass == null) {
      throw new RouteHasNoTargetException("Missing target for component '"
          + componentClass.getName() + "' registered for route: "
          + registry.getRoute(componentClass) + ". Make sure to provide a valid target component.");
    }

    if (!Frame.class.isAssignableFrom(targetClass)) {
      Component parent = render(targetClass);
      clearTargetContent(parent);
      Component componentInstance = getOrCreateComponent(componentClass);

      // Render the component in the parent component
      if (parent instanceof RouteTarget) {
        ((RouteTarget) parent).showRouteContent(componentInstance);
      } else if (parent instanceof HasComponents) {
        ((HasComponents) parent).add(componentInstance);
      } else {
        throw new RouteRenderException("Cannot render route's component in parent "
            + "that does not implement RouteTarget or HasComponents interface. "
            + "Trying to render component '" + componentClass.getName() + "' "
            + "in parent, registered for " + registry.getRoute(componentClass) + " : "
            + parent.getClass().getName());

      }

      // Track the current view for the parent
      currentViews.put(parent, componentInstance);
    } else {
      // The component's parent is a Frame
      // We need to find the Frame instance to add the component to
      Frame frame = getFrameComponent(componentClass);
      if (frame == null) {
        throw new RouteHasNoTargetException(
            "No route target found for component: " + componentClass.getName()
                + ", route is registered as " + registry.getRoute(componentClass)
                + " Trying to render component in a Frame but no Frame is available."
                + " Make sure to provide a valid Frame target for the component.");
      }

      clearTargetContent(frame);
      Component componentInstance = getOrCreateComponent(componentClass);
      frame.add(componentInstance);

      // Track the current view for the parent
      currentViews.put(frame, componentInstance);
    }

    return viewCache.get(componentClass);
  }

  private Component getOrCreateComponent(Class<? extends Component> componentClass) {
    // Use the cache in case the component has already been created
    Component componentInstance = viewCache.get(componentClass);
    if (componentInstance == null || componentInstance.isDestroyed()) {
      // try to create the component instance
      try {
        componentInstance = componentClass.getDeclaredConstructor().newInstance();
        viewCache.put(componentClass, componentInstance);

        componentInstance.addLifecycleObserver((component, event) -> {
          if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
            viewCache.remove(componentClass);
            currentViews.values().remove(component);
          }
        });
      } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
          | InvocationTargetException | NoSuchMethodException | SecurityException e) {
        throw new RouteRenderException("Failed to render component instance: " + componentClass, e);
      }
    }

    return componentInstance;
  }

  private void clearTargetContent(Object parent) {
    Component currentView = currentViews.get(parent);
    if (currentView != null) {
      if (parent instanceof RouteTarget) {
        ((RouteTarget) parent).removeRouteContent(currentView);
      } else if (parent instanceof HasComponents) {
        ((HasComponents) parent).remove(currentView);
      } else {
        throw new RouteRenderException("Cannot clear target content in parent "
            + "that does not implement RouteTarget or HasComponents interface. "
            + "Trying to clear content of component '" + currentView.getClass().getName() + "' "
            + "in parent: " + parent.getClass().getName());
      }

      // Remove tracking for the cleared view
      currentViews.remove(parent);
    }
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
