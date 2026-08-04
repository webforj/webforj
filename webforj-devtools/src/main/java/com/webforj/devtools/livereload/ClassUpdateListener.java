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
 * own application instance. When every changed class is accounted for by the route tree, the router
 * recreates the affected part of the active hierarchy in place, so the rest of the interface and
 * the application state survive the change. An application without routing, a class the route tree
 * does not know, or a vetoed recreation all end in the full page reload, because nothing is ever
 * guessed about code the route tree cannot account for.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ClassUpdateListener implements AppLifecycleListener {

  static final String EVENT_TYPE = "webforj-devtools-class-update";
  static final String DATA_KEY = "classes";

  private static final System.Logger logger = System.getLogger(ClassUpdateListener.class.getName());

  private final Gson gson = new Gson();

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

    // The hierarchy iterates root first, so the first hit is the topmost affected node and its
    // recreation covers every changed class below it.
    Class<? extends Component> target = null;
    Set<String> partOfHierarchy = new HashSet<>();
    for (RouteRelation<Class<? extends Component>> node : activePath.get()) {
      Class<? extends Component> nodeClass = node.getData();
      if (classNames.contains(nodeClass.getName())) {
        if (target == null) {
          target = nodeClass;
        }

        partOfHierarchy.add(nodeClass.getName());
      }
    }

    for (String className : classNames) {
      if (!partOfHierarchy.contains(className) && !isRegisteredRoute(router, className)) {
        reloadPage(page, "the class " + className + " is outside the route tree");
        return;
      }
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

  private static boolean isRegisteredRoute(Router router, String className) {
    for (RouteEntry entry : router.getRegistry().getAvailableRouteEntires()) {
      if (entry.getComponent().getName().equals(className)) {
        return true;
      }
    }

    return false;
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
