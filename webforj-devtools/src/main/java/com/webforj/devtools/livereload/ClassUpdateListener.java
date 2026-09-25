package com.webforj.devtools.livereload;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.typesafe.config.Config;
import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.component.Component;
import com.webforj.event.page.PageEvent;
import com.webforj.event.page.PageEventOptions;
import com.webforj.router.NavigationOptions;
import com.webforj.router.RouteEntry;
import com.webforj.router.RouteRelation;
import com.webforj.router.Router;
import com.webforj.router.history.Location;
import java.io.UncheckedIOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

/**
 * Rebuilds the part of the interface a class update names, or reloads the page when the change
 * cannot be mapped onto the route tree.
 *
 * <p>
 * The reload client hands every class update to the page, and the page raises it here, inside its
 * own application instance. A changed class is accounted for when it is a route or when the
 * compiled code of a rendered route references it, directly or through other application classes.
 * When every changed class is accounted for, the router recreates the affected part of the active
 * hierarchy in place, so the rest of the interface and the application state survive the change. An
 * application without routing, a class no rendered route reaches, or a vetoed recreation all end in
 * the full page reload.
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

  /**
   * Creates a listener that reads the class references through the index every page shares.
   */
  public ClassUpdateListener() {
    this(sharedReferences);
  }

  ClassUpdateListener(ClassReferenceIndex references) {
    this.references = references;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
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

    // The hierarchy iterates root first, so the first affected node is the topmost one and its
    // recreation covers every changed class below it. A node is affected when it is a changed
    // route or when its compiled code reaches a changed class, directly or through other
    // application classes. The walk stops at other routes, because the router creates a route and
    // a reference to one, a navigation target for example, never builds it. A change to routes
    // alone is complete before any class file is read.
    Set<String> routes = getRegisteredRoutes(router);
    Class<? extends Component> target = null;
    ClassReferenceIndex.Walk walk;
    try {
      Class<? extends Component> root = activePath.get().getData();
      walk = references.newWalk(root.getClassLoader(), classNames, routes);
      for (RouteRelation<Class<? extends Component>> node : activePath.get()) {
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
    } catch (UncheckedIOException e) {
      logger.log(System.Logger.Level.DEBUG, "Could not read the class references", e);
      reloadPage(page, e.getMessage());
      return;
    }

    if (!walk.isComplete()) {
      reloadPage(page, "the class " + walk.getUnreached().iterator().next()
          + " is outside the rendered route tree");
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
    router.navigate(location.get(), options, rendered -> {
      if (!rendered.isPresent()) {
        reloadPage(page, "the affected part could not be rebuilt");
      }
    });
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

  private static Set<String> getRegisteredRoutes(Router router) {
    Set<String> routes = new HashSet<>();
    for (RouteEntry entry : router.getRegistry().getAvailableRouteEntires()) {
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
