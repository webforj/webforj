package com.webforj.devtools.livereload;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.annotation.Routify;
import com.webforj.component.Component;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRegistry;
import com.webforj.router.RouteRelation;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import java.io.UncheckedIOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

/**
 * Rebuilds the part of the interface a class update names, or reloads the page when the change
 * cannot be mapped onto the route tree.
 *
 * <p>
 * The reload client hands every class update to the page, and the page raises it here, inside its
 * own application instance. A changed class is accounted for when it is a route or when the
 * compiled code of a rendered route references it, directly or through other application classes.
 * Before the rebuild, the routes in the route packages of the application follow the class folders.
 * A route whose class file is gone is unregistered until the file returns, a changed route is
 * registered again from its annotations, and a route the router does not know yet, changed or
 * referenced by a changed class, is discovered in the route packages and registered. When every
 * changed class is accounted for, the router recreates the affected part of the active hierarchy in
 * place, so the rest of the interface and the application state survive the change. An application
 * without routing, a class no rendered route reaches, a route update that fails, or a vetoed
 * recreation all end in the full page reload.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ClassUpdateListener implements AppLifecycleListener {

  static final String EVENT_TYPE = "webforj-devtools-class-update";
  static final String DATA_KEY = "classes";

  private static final System.Logger logger = System.getLogger(ClassUpdateListener.class.getName());
  private static final ClassReferenceIndex sharedReferences = new ClassReferenceIndex();

  private final Gson gson = new Gson();
  private final ClassReferenceIndex references;
  private final Function<String[], RouteRegistry> routeDiscovery;
  private final Map<String, Optional<Path>> routeClassFiles = new HashMap<>();
  private final Map<String, Path> goneRoutes = new HashMap<>();
  private String[] routePackages = new String[0];

  /**
   * Creates a listener that reads the class references through the index every page shares.
   */
  public ClassUpdateListener() {
    this(sharedReferences);
  }

  ClassUpdateListener(ClassReferenceIndex references) {
    this(references, RouteRegistry::ofPackage);
  }

  ClassUpdateListener(ClassReferenceIndex references,
      Function<String[], RouteRegistry> routeDiscovery) {
    this.references = references;
    this.routeDiscovery = routeDiscovery;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
    routePackages = readRoutePackages(app);
    register(LiveReloadOptions.from(getConfig()), Page.getCurrent());
  }

  void register(LiveReloadOptions options, Page page) {
    if (!options.isEnabled() || page == null) {
      return;
    }

    PageEventOptions eventOptions =
        new PageEventOptions().addData(DATA_KEY, "JSON.stringify(event.detail.classes)");
    page.addEventListener(EVENT_TYPE, this::handleUpdate, eventOptions);
    logger.log(System.Logger.Level.DEBUG, "webforJ class update listener registered for the page");
  }

  void handleUpdate(PageEvent event) {
    Set<String> classNames = readClassNames(event);
    if (classNames.isEmpty()) {
      return;
    }

    apply(classNames, Router.getCurrent(), Page.getCurrent());
  }

  void apply(Set<String> classNames, Router router, Page page) {
    if (router == null) {
      reloadPage(page, "the application runs without routing");
      return;
    }

    Optional<RouteRelation<Class<? extends Component>>> activePath =
        router.getRenderer().getActiveRoutePath();
    if (!activePath.isPresent()) {
      reloadPage(page, "no route is rendered");
      return;
    }

    RouteRegistry registry = router.getRegistry();
    ClassReferenceIndex.Update update;
    Set<String> movedRoutes;
    try {
      update = references.newUpdate(activePath.get().getData().getClassLoader(), classNames);
      movedRoutes = updateRoutes(classNames, registry, update);
    } catch (RuntimeException | LinkageError e) {
      logger.log(System.Logger.Level.DEBUG, "Could not update the routes", e);
      reloadPage(page, "the routes could not be updated, " + e.getMessage());
      return;
    }

    // The hierarchy iterates root first, so the first affected node is the topmost one and its
    // recreation covers every changed class below it. A node is affected when it is a changed
    // route or when its compiled code reaches a changed class, directly or through other
    // application classes. The walk stops at other routes, because the router creates a route and
    // a reference to one, a navigation target for example, never builds it. A route whose paths
    // moved is not a boundary, because a node that links to it built the link from the old path.
    // A change to routes alone, their paths kept, is complete before any class file is read.
    Set<String> routes = getRegisteredRoutes(registry);
    routes.removeAll(movedRoutes);
    ClassReferenceIndex.Walk walk;
    Class<? extends Component> target;
    try {
      walk = update.newWalk(routes);
      target = findRebuildTarget(classNames, activePath.get(), walk);
    } catch (UncheckedIOException e) {
      logger.log(System.Logger.Level.DEBUG, "Could not read the class references", e);
      reloadPage(page, e.getMessage());
      return;
    }

    Set<String> unreached = walk.getUnreached();
    unreached.removeAll(movedRoutes);
    if (!unreached.isEmpty()) {
      reloadPage(page,
          "the class " + unreached.iterator().next() + " is outside the rendered route tree");
      return;
    }

    if (target == null) {
      logger.log(System.Logger.Level.DEBUG,
          "The changed classes are routes this page does not render, nothing to rebuild");
      return;
    }

    Optional<Location> location = router.getResolvedLocation();
    if (!location.isPresent()) {
      reloadPage(page, "no location is resolved");
      return;
    }

    logger.log(System.Logger.Level.DEBUG,
        "Rebuilding the route part from " + target.getName() + " for a class update");
    NavigationOptions options =
        new NavigationOptions().setUpdateHistory(false).setRecreateFrom(target);
    try {
      router.navigate(location.get(), options, rendered -> {
        if (!rendered.isPresent()) {
          reloadPage(page, "the affected part could not be rebuilt");
        }
      });
    } catch (RuntimeException e) {
      logger.log(System.Logger.Level.DEBUG, "Could not rebuild the affected part", e);
      reloadPage(page, "the affected part could not be rebuilt, " + e.getMessage());
    }
  }

  private Set<String> readClassNames(PageEvent event) {
    Object value = event.getData().get(DATA_KEY);
    if (!(value instanceof String json) || json.isBlank()) {
      return Collections.emptySet();
    }

    try {
      String[] names = gson.fromJson(json, String[].class);
      if (names == null) {
        return Collections.emptySet();
      }

      Set<String> classNames = new HashSet<>();
      for (String name : names) {
        if (name != null && !name.isBlank()) {
          classNames.add(name);
        }
      }

      return classNames;
    } catch (JsonSyntaxException e) {
      logger.log(System.Logger.Level.DEBUG, "Ignoring an unreadable class update", e);
      return Collections.emptySet();
    }
  }

  private Set<String> updateRoutes(Set<String> classNames, RouteRegistry registry,
      ClassReferenceIndex.Update update) {
    if (routePackages.length == 0) {
      return Collections.emptySet();
    }

    removeGoneRoutes(registry);
    Set<String> candidates = findReturningRoutes();
    Set<String> movedRoutes = applyChangedRoutes(classNames, registry, update);
    candidates.addAll(findNewRoutes(classNames, getRegisteredRoutes(registry), update));
    addDiscoveredRoutes(candidates, registry);

    return movedRoutes;
  }

  private void removeGoneRoutes(RouteRegistry registry) {
    Set<String> checked = new HashSet<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      String className = entry.getComponent().getName();
      if (isInRoutePackages(className) && checked.add(className)) {
        removeGoneRoute(registry, entry);
      }
    }
  }

  private void removeGoneRoute(RouteRegistry registry, RouteEntry entry) {
    Class<? extends Component> component = entry.getComponent();
    String className = component.getName();
    if (!routeClassFiles.containsKey(className)) {
      Optional<URL> location =
          ClassReferenceIndex.findClassFile(component.getClassLoader(), className);
      if (location.isEmpty()) {
        removeRoute(registry, component);
        logger.log(System.Logger.Level.DEBUG, "Unregistered the route " + entry.getPath() + " of "
            + className + ", its class file is gone");
        return;
      }

      routeClassFiles.put(className,
          ClassReferenceIndex.toClassFolderFile(location.get(), className));
    }

    Optional<Path> classFile = routeClassFiles.get(className);
    if (classFile.isPresent() && !Files.exists(classFile.get())) {
      removeRoute(registry, component);
      routeClassFiles.remove(className);
      goneRoutes.put(className, classFile.get());
      logger.log(System.Logger.Level.DEBUG, "Unregistered the route " + entry.getPath() + " of "
          + className + ", its class file is gone");
    }
  }

  private Set<String> findReturningRoutes() {
    Set<String> returning = new HashSet<>();
    Iterator<Map.Entry<String, Path>> gone = goneRoutes.entrySet().iterator();
    while (gone.hasNext()) {
      Map.Entry<String, Path> route = gone.next();
      if (Files.exists(route.getValue())) {
        returning.add(route.getKey());
        routeClassFiles.put(route.getKey(), Optional.of(route.getValue()));
        gone.remove();
      }
    }

    return returning;
  }

  private Set<String> applyChangedRoutes(Set<String> classNames, RouteRegistry registry,
      ClassReferenceIndex.Update update) {
    Map<String, Class<? extends Component>> changed = new HashMap<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      String className = entry.getComponent().getName();
      if (classNames.contains(className) && !changed.containsKey(className)
          && isInRoutePackages(className) && update.isRoute(className)) {
        changed.put(className, entry.getComponent());
      }
    }

    Set<String> movedRoutes = new HashSet<>();
    Deque<Class<? extends Component>> movedOutlets = new ArrayDeque<>();
    for (Class<? extends Component> component : changed.values()) {
      if (updateRoute(registry, component)) {
        movedRoutes.add(component.getName());
        movedOutlets.add(component);
      }
    }

    while (!movedOutlets.isEmpty()) {
      for (Class<? extends Component> child : findChildRoutes(registry, movedOutlets.pop(),
          update)) {
        if (!movedRoutes.contains(child.getName()) && updateRoute(registry, child)) {
          movedRoutes.add(child.getName());
          movedOutlets.add(child);
        }
      }
    }

    return movedRoutes;
  }

  private Set<Class<? extends Component>> findChildRoutes(RouteRegistry registry,
      Class<? extends Component> outlet, ClassReferenceIndex.Update update) {
    Set<Class<? extends Component>> children = new HashSet<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      String className = entry.getComponent().getName();
      if (outlet.equals(entry.getOutlet()) && isInRoutePackages(className)
          && update.isRoute(className)) {
        children.add(entry.getComponent());
      }
    }

    return children;
  }

  private Set<String> findNewRoutes(Set<String> classNames, Set<String> known,
      ClassReferenceIndex.Update update) {
    Set<String> candidates = new HashSet<>();
    for (String className : classNames) {
      try {
        addRouteCandidate(candidates, className, known, update);
        for (String reference : update.getReferences(className)) {
          addRouteCandidate(candidates, reference, known, update);
        }
      } catch (UncheckedIOException e) {
        logger.log(System.Logger.Level.DEBUG, "Could not read the class references of " + className,
            e);
        if (isInRoutePackages(className) && !known.contains(className)) {
          candidates.add(className);
        }
      }
    }

    return candidates;
  }

  private void addRouteCandidate(Set<String> candidates, String className, Set<String> known,
      ClassReferenceIndex.Update update) {
    if (isInRoutePackages(className) && !known.contains(className) && update.isRoute(className)) {
      candidates.add(className);
    }
  }

  private void addDiscoveredRoutes(Set<String> candidates, RouteRegistry registry) {
    if (candidates.isEmpty()) {
      return;
    }

    for (RouteEntry entry : routeDiscovery.apply(routePackages).getAvailableRouteEntires()) {
      String componentName = entry.getComponent().getName();
      if (candidates.contains(componentName)) {
        registry.register(entry);
        logger.log(System.Logger.Level.DEBUG,
            "Registered the route " + entry.getPath() + " of " + componentName);
      }
    }
  }

  private boolean isInRoutePackages(String className) {
    for (String routePackage : routePackages) {
      if (className.startsWith(routePackage + ".")) {
        return true;
      }
    }

    return false;
  }

  private static Class<? extends Component> findRebuildTarget(Set<String> classNames,
      RouteRelation<Class<? extends Component>> activePath, ClassReferenceIndex.Walk walk) {
    Class<? extends Component> target = null;
    for (RouteRelation<Class<? extends Component>> node : activePath) {
      Class<? extends Component> nodeClass = node.getData();
      // A changed route still walks, so a class only that route reaches is accounted for.
      boolean reaches = !walk.isComplete() && !walk.reach(nodeClass.getName()).isEmpty();
      boolean affected = classNames.contains(nodeClass.getName()) || reaches;
      if (affected && target == null) {
        target = nodeClass;
      }

      if (target != null && walk.isComplete()) {
        break;
      }
    }

    return target;
  }

  private static boolean updateRoute(RouteRegistry registry, Class<? extends Component> component) {
    Set<String> previousPaths = getRoutePaths(registry, component);
    removeRoute(registry, component);
    registry.register(component);
    boolean moved = !previousPaths.equals(getRoutePaths(registry, component));
    logger.log(System.Logger.Level.DEBUG,
        "Registered the route of " + component.getName() + " again from its class");

    return moved;
  }

  private static void removeRoute(RouteRegistry registry, Class<? extends Component> component) {
    Set<String> paths = getRoutePaths(registry, component);
    List<RouteEntry> sharing = new ArrayList<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      if (!entry.getComponent().equals(component) && paths.contains(entry.getPath())) {
        sharing.add(entry);
      }
    }

    for (String path : paths) {
      registry.unregister(path);
    }

    for (RouteEntry entry : sharing) {
      registry.register(entry);
    }
  }

  private static Set<String> getRoutePaths(RouteRegistry registry,
      Class<? extends Component> component) {
    Set<String> paths = new HashSet<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      if (entry.getComponent().equals(component)) {
        paths.add(entry.getPath());
      }
    }

    return paths;
  }

  private static String[] readRoutePackages(App app) {
    Routify routify = app.getClass().getAnnotation(Routify.class);
    if (routify == null) {
      return new String[0];
    }

    String[] packages = routify.packages();
    return packages.length == 0 ? new String[] {app.getClass().getPackageName()} : packages;
  }

  private static Set<String> getRegisteredRoutes(RouteRegistry registry) {
    Set<String> routes = new HashSet<>();
    for (RouteEntry entry : registry.getAvailableRouteEntires()) {
      routes.add(entry.getComponent().getName());
    }

    return routes;
  }

  private static void reloadPage(Page page, String reason) {
    if (page == null) {
      return;
    }

    logger.log(System.Logger.Level.INFO, "Reloading the page for a class update, " + reason);
    page.reload();
  }

  private static Config getConfig() {
    Environment env = Environment.getCurrent();
    return env != null ? env.getConfig() : null;
  }
}
