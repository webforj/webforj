package com.webforj.devtools.craftforj.router;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.router.model.ActiveRouteState;
import com.webforj.dispatcher.ListenerRegistration;
import com.webforj.router.RoutePathResolver;
import com.webforj.router.RoutePattern;
import com.webforj.router.Router;
import com.webforj.router.event.NavigateEvent;
import com.webforj.router.history.Location;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Tracks the active route state from the router.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ActiveRouteTracker {

  private ListenerRegistration<?> registration;
  private volatile ActiveRouteState currentState;
  private Router router;

  /**
   * Attaches to the router to track the active route.
   *
   * <p>
   * A second attach releases the previous listener registration first, so trackers never stack.
   * </p>
   *
   * @param router the router to attach to
   */
  public void attach(Router router) {
    if (router == null) {
      return;
    }

    detach();
    this.router = router;
    registration = router.addNavigateListener(this::onNavigate);

    // Initialize state from current location if navigation already happened
    initializeFromCurrentLocation();
  }

  /**
   * Detaches from the router.
   */
  public void detach() {
    if (registration != null) {
      registration.remove();
      registration = null;
    }
    router = null;
    currentState = null;
  }

  /**
   * Gets the current active route state.
   *
   * @return current state, or null if not yet navigated
   */
  public ActiveRouteState getCurrentState() {
    return currentState;
  }

  private void onNavigate(NavigateEvent event) {
    String path = event.getContext().getLocation().getFullURI();

    ActiveRouteState state = new ActiveRouteState();
    state.setCurrentPath(path);
    state.setParams(event.getContext().getRouteParameters().all());
    state.setQueryParams(event.getContext().getLocation().getQueryParameters().all());
    state.setFragment(event.getContext().getLocation().getFragment());

    if (event.getContext().getComponent() != null) {
      List<String> activeIds = buildActiveRouteIds(event.getContext().getComponent().getClass());
      state.setActiveRouteIds(activeIds);
    }

    currentState = state;
  }

  private List<String> buildActiveRouteIds(Class<?> componentClass) {
    List<String> ids = new ArrayList<>();
    Router r = this.router != null ? this.router : Router.getCurrent();
    if (r == null) {
      return ids;
    }

    r.getRegistry().getComponentHierarchy(componentClass.asSubclass(Component.class))
        .ifPresent(hierarchy -> {
          hierarchy.forEach(node -> {
            Class<? extends Component> comp = node.getData();
            String path = RoutePathResolver.resolvePath(comp);
            if (path != null) {
              ids.add(comp.getName() + ":" + path);
            }
          });
        });

    return ids;
  }

  private void initializeFromCurrentLocation() {
    Optional<Location> location = router.getResolvedLocation();
    if (!location.isPresent()) {
      return;
    }

    Location loc = location.get();
    ActiveRouteState state = new ActiveRouteState();
    state.setCurrentPath(loc.getFullURI());
    state.setQueryParams(loc.getQueryParameters().all());
    state.setFragment(loc.getFragment());

    // Get the component class from the route pattern
    Optional<RoutePattern> pattern = router.getRoutePatternByLocation(loc);
    if (pattern.isPresent()) {
      Optional<Class<? extends Component>> componentClass =
          router.getRegistry().getComponentByRoute(pattern.get().getPattern());

      if (componentClass.isPresent()) {
        List<String> activeIds = buildActiveRouteIds(componentClass.get());
        state.setActiveRouteIds(activeIds);

        // Extract route parameters from the pattern
        state.setParams(pattern.get().getParameters(loc.getSegments().getPath()));
      } else {
        state.setActiveRouteIds(Collections.emptyList());
        state.setParams(Collections.emptyMap());
      }
    } else {
      state.setActiveRouteIds(Collections.emptyList());
      state.setParams(Collections.emptyMap());
    }

    currentState = state;
  }
}
