package com.webforj.devtools.craftforj.router;

import com.webforj.component.Component;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourceFileResolver;
import com.webforj.devtools.craftforj.inspector.source.resolver.SourcePathRegistry;
import com.webforj.devtools.craftforj.router.model.ActiveRouteState;
import com.webforj.devtools.craftforj.router.model.RouteAliasInfo;
import com.webforj.devtools.craftforj.router.model.RouteInfo;
import com.webforj.devtools.craftforj.router.model.RouteType;
import com.webforj.devtools.craftforj.router.model.SecurityAccess;
import com.webforj.devtools.craftforj.utilities.KotlinClassDetector;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.Router;
import com.webforj.router.annotation.FrameTitle;
import com.webforj.router.annotation.Route;
import com.webforj.router.annotation.RouteAlias;
import com.webforj.router.observer.ActivateObserver;
import com.webforj.router.observer.DidEnterObserver;
import com.webforj.router.observer.DidLeaveObserver;
import com.webforj.router.observer.WillEnterObserver;
import com.webforj.router.observer.WillLeaveObserver;
import com.webforj.router.security.annotation.AnonymousAccess;
import jakarta.annotation.security.DenyAll;
import jakarta.annotation.security.PermitAll;
import jakarta.annotation.security.RolesAllowed;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Collects route information from the router.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class RouteCollector {

  private final Router router;
  private final ActiveRouteTracker activeRouteTracker;

  /**
   * Creates a collector with a specific router and active route tracker.
   *
   * @param router the router to use
   * @param activeRouteTracker the tracker that knows the active routes, may be {@code null}
   */
  public RouteCollector(Router router, ActiveRouteTracker activeRouteTracker) {
    this.router = router;
    this.activeRouteTracker = activeRouteTracker;
  }

  /**
   * Creates a collector with a specific router.
   *
   * @param router the router to use
   */
  public RouteCollector(Router router) {
    this(router, null);
  }

  /** Creates a collector using the current router. */
  public RouteCollector() {
    this(null, null);
  }

  /**
   * Collects all routes from the router registry as a tree.
   *
   * @return list of top-level RouteInfo with nested children
   */
  public List<RouteInfo> collectRoutes() {
    Router r = this.router != null ? this.router : Router.getCurrent();
    if (r == null) {
      return Collections.emptyList();
    }

    RouteRegistry registry = r.getRegistry();
    if (registry == null) {
      return Collections.emptyList();
    }

    List<RouteEntry> entries = registry.getAvailableRouteEntires();

    // First: create all route info objects. A component class can be registered under several
    // route entries (aliases, multiple paths), so ids are grouped per class instead of overwritten.
    Map<String, RouteInfo> routeMap = new HashMap<>();
    Map<String, List<String>> componentToIds = new HashMap<>();

    for (RouteEntry entry : entries) {
      RouteInfo info = createRouteInfo(entry);
      routeMap.put(info.getId(), info);
      componentToIds.computeIfAbsent(info.getComponentType(), k -> new ArrayList<>())
          .add(info.getId());
    }

    // Second: build tree by matching outletType to every parent entry of that component type
    Map<String, List<RouteInfo>> childrenMap = new HashMap<>();
    Set<String> childIds = new HashSet<>();

    for (RouteInfo route : routeMap.values()) {
      for (String parentId : componentToIds.getOrDefault(route.getOutletType(),
          Collections.emptyList())) {
        if (!parentId.equals(route.getId())) {
          childrenMap.computeIfAbsent(parentId, k -> new ArrayList<>()).add(route);
          childIds.add(route.getId());
        }
      }
    }

    // Third: assign children to parents
    for (Map.Entry<String, List<RouteInfo>> entry : childrenMap.entrySet()) {
      RouteInfo parent = routeMap.get(entry.getKey());
      if (parent != null) {
        List<RouteInfo> children = entry.getValue();
        children.sort((a, b) -> a.getPath().compareTo(b.getPath()));
        parent.setChildren(children);
      }
    }

    // Collect top-level routes
    List<RouteInfo> result = new ArrayList<>();
    for (RouteInfo route : routeMap.values()) {
      if (!childIds.contains(route.getId())) {
        result.add(route);
      }
    }

    result.sort((a, b) -> a.getPath().compareTo(b.getPath()));

    return Collections.unmodifiableList(result);
  }

  private RouteInfo createRouteInfo(RouteEntry entry) {
    Class<? extends Component> componentClass = entry.getComponent();
    String path = entry.getPath();

    RouteInfo info = new RouteInfo();
    info.setId(generateRouteId(path, componentClass));
    info.setPath(path);
    info.setComponentType(componentClass.getName());
    info.setDisplayName(componentClass.getSimpleName());
    info.setType(extractRouteType(path, componentClass));
    info.setOutletType(entry.getOutlet().getName());
    info.setFrameId(entry.getFrameId().orElse(null));
    info.setPriority(entry.getPriority());
    info.setFrameTitle(extractFrameTitle(componentClass));
    info.setAliases(extractAliases(componentClass));
    info.setParams(RoutePatternParser.parse(path));
    info.setSecurity(extractSecurityAccess(componentClass));
    info.setAllowedRoles(extractAllowedRoles(componentClass));
    info.setHasWillEnter(WillEnterObserver.class.isAssignableFrom(componentClass));
    info.setHasDidEnter(DidEnterObserver.class.isAssignableFrom(componentClass));
    info.setHasWillLeave(WillLeaveObserver.class.isAssignableFrom(componentClass));
    info.setHasDidLeave(DidLeaveObserver.class.isAssignableFrom(componentClass));
    info.setHasActivate(ActivateObserver.class.isAssignableFrom(componentClass));
    String sourceFile =
        SourceFileResolver.resolve(componentClass.getName(), SourceFileResolver.ALL_EXTENSIONS);
    SourcePathRegistry.addPath(sourceFile);
    info.setSourceFile(sourceFile);
    info.setKotlin(KotlinClassDetector.isKotlin(componentClass));
    info.setActive(isActive(info.getId()));

    return info;
  }

  private boolean isActive(String routeId) {
    if (activeRouteTracker == null) {
      return false;
    }

    ActiveRouteState state = activeRouteTracker.getCurrentState();

    return state != null && state.getActiveRouteIds() != null
        && state.getActiveRouteIds().contains(routeId);
  }

  private String generateRouteId(String path, Class<? extends Component> componentClass) {
    return componentClass.getName() + ":" + path;
  }

  private RouteType extractRouteType(String path, Class<? extends Component> componentClass) {
    if (path.startsWith("@")) {
      return RouteType.LAYOUT;
    }

    Route routeAnnotation = componentClass.getAnnotation(Route.class);
    if (routeAnnotation != null) {
      Route.Type annotationType = routeAnnotation.type();
      if (annotationType == Route.Type.LAYOUT) {
        return RouteType.LAYOUT;
      } else if (annotationType == Route.Type.VIEW) {
        return RouteType.VIEW;
      }
      if (componentClass.getSimpleName().endsWith("Layout")) {
        return RouteType.LAYOUT;
      }
    }

    return RouteType.VIEW;
  }

  private String extractFrameTitle(Class<? extends Component> componentClass) {
    FrameTitle annotation = componentClass.getAnnotation(FrameTitle.class);
    return annotation != null ? annotation.value() : null;
  }

  private List<RouteAliasInfo> extractAliases(Class<? extends Component> componentClass) {
    RouteAlias[] aliases = componentClass.getAnnotationsByType(RouteAlias.class);
    if (aliases.length == 0) {
      return Collections.emptyList();
    }

    List<RouteAliasInfo> result = new ArrayList<>();
    for (RouteAlias alias : aliases) {
      RouteAliasInfo info = new RouteAliasInfo();
      info.setPath(alias.value());
      info.setPriority(alias.priority());
      result.add(info);
    }

    return Collections.unmodifiableList(result);
  }

  private SecurityAccess extractSecurityAccess(Class<? extends Component> componentClass) {
    if (componentClass.isAnnotationPresent(PermitAll.class)) {
      return SecurityAccess.PERMIT_ALL;
    }
    if (componentClass.isAnnotationPresent(DenyAll.class)) {
      return SecurityAccess.DENY_ALL;
    }
    if (componentClass.isAnnotationPresent(RolesAllowed.class)) {
      return SecurityAccess.ROLES_ALLOWED;
    }
    if (componentClass.isAnnotationPresent(AnonymousAccess.class)) {
      return SecurityAccess.ANONYMOUS;
    }

    return SecurityAccess.NONE;
  }

  private List<String> extractAllowedRoles(Class<? extends Component> componentClass) {
    RolesAllowed annotation = componentClass.getAnnotation(RolesAllowed.class);
    if (annotation == null) {
      return Collections.emptyList();
    }

    return Collections.unmodifiableList(Arrays.asList(annotation.value()));
  }
}
