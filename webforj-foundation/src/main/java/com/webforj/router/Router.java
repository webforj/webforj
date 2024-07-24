package com.webforj.router;

import static com.webforj.App.console;

import com.webforj.App;
import com.webforj.component.Component;
import com.webforj.component.ComponentLifecycleObserver;
import com.webforj.component.window.Frame;
import com.webforj.concern.HasComponents;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Router class responsible for navigating and rendering components based on registered routes.
 */
public class Router {
  private final RouteRegistry registry;
  private final Map<Class<? extends Component>, Component> viewCache;
  private final Map<Object, Component> currentViews; // Track current views for each parent

  public Router(RouteRegistry registry) {
    this.registry = registry;
    this.viewCache = new HashMap<>();
    this.currentViews = new HashMap<>();
  }

  public void navigate(String path) {
    Class<? extends Component> componentClass = registry.getComponent(path);
    if (componentClass != null) {
      try {
        createAndRenderComponent(componentClass);
      } catch (InstantiationException | IllegalAccessException | NoSuchMethodException
          | InvocationTargetException e) {
        App.console().log(e);
        renderNotFound();
      }
    } else {
      renderNotFound();
    }
  }

  private Component createAndRenderComponent(Class<? extends Component> componentClass)
      throws InstantiationException, IllegalAccessException, NoSuchMethodException,
      InvocationTargetException {

    // We don't want to create Frame instances. They are created by the developer
    if (Frame.class.isAssignableFrom(componentClass)) {
      throw new IllegalStateException("Cannot create Frame instances");
    }

    Component componentInstance = viewCache.get(componentClass);
    // Use the cache in case the component has already been created
    if (componentInstance == null || componentInstance.isDestroyed()) {
      // Create the component instance
      componentInstance = componentClass.getDeclaredConstructor().newInstance();
      viewCache.put(componentClass, componentInstance);

      String componentId = componentInstance.getComponentId();
      // Add lifecycle observer to handle destruction
      componentInstance.addLifecycleObserver((component, event) -> {
        if (event == ComponentLifecycleObserver.LifecycleEvent.DESTROY) {
          viewCache.remove(componentClass);
          currentViews.values().remove(component);
          console().log("Component destroyed: " + componentId);
        }
      });
    }

    // Check if the component has a parent (target)
    Class<? extends Component> targetClass = registry.getTarget(componentClass);
    if (targetClass != null && !Frame.class.isAssignableFrom(targetClass)) {
      // Create the parent component if it hasn't been created yet
      Component parent = createAndRenderComponent(targetClass);

      // Clear only the target part of the parent component
      clearTargetContent(parent);

      // Render the component in the parent component
      if (parent instanceof RouteTarget) {
        ((RouteTarget) parent).showRouteContent(componentInstance);
      } else if (parent instanceof HasComponents) {
        ((HasComponents) parent).add(componentInstance);
      } else {
        throw new IllegalStateException(
            "Target must implement either RouteTarget or HasComponents: " + targetClass.getName());
      }
      currentViews.put(parent, componentInstance); // Track the current view for the parent
    } else {
      // The component parent is a Frame
      // We need to find the Frame instance to add the component to
      Frame frame = getFrameComponent(componentClass);
      if (frame != null) {
        clearTargetContent(frame);
        frame.add(componentInstance);
        currentViews.put(frame, componentInstance); // Track the current view for the frame
      } else {
        throw new IllegalStateException(
            "No Frame found for component: " + componentClass.getName());
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
        throw new IllegalStateException(
            "Parent must implement either RouteTarget or HasComponents: "
                + parent.getClass().getName());
      }
      currentViews.remove(parent); // Remove tracking for the cleared view
      viewCache.remove(currentView.getClass()); // Remove the cleared view from the cache
    }
  }

  private Frame getFrameComponent(Class<? extends Component> componentClass) {
    List<Frame> frames = App.getFrames();
    String frameId = registry.getFrameId(componentClass);

    if (frameId != null) {
      for (Frame frame : frames) {
        if (frame.getFrameId().equals(frameId)) {
          return frame;
        }
      }
    } else {
      for (Frame frame : frames) {
        if (!frame.getFrameId().equals("com.webforj.utilities.WelcomeApp")) {
          return frame;
        }
      }
    }

    return null;
  }

  private void renderNotFound() {
    App.console().log("404 - Not Found");
    System.out.println("404 - Not Found");
  }
}
