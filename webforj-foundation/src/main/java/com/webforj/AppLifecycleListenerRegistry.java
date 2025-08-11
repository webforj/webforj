package com.webforj;

import com.webforj.annotation.AppListenerPriority;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

/**
 * Registry for App lifecycle listeners using Java ServiceLoader mechanism.
 *
 * <p>
 * This class manages the discovery and lifecycle of {@link AppLifecycleListener} implementations
 * using the standard Java {@link ServiceLoader} mechanism. Each App instance gets its own set of
 * listener instances to ensure proper isolation.
 * </p>
 *
 * <p>
 * To register a listener, implementations must:
 * <ol>
 * <li>Implement the {@link AppLifecycleListener} interface</li>
 * <li>Be listed in {@code META-INF/services/com.webforj.AppLifecycleListener}</li>
 * <li>Optionally use {@link AppListenerPriority} annotation to control execution order</li>
 * </ol>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 25.02
 */
final class AppLifecycleListenerRegistry {
  private static final Logger logger =
      System.getLogger(AppLifecycleListenerRegistry.class.getName());

  // Track which App instances have which listeners
  private static final Map<App, List<AppLifecycleListener>> appListeners =
      new ConcurrentHashMap<>();

  private AppLifecycleListenerRegistry() {
    // Prevent instantiation
  }

  /**
   * Registers lifecycle listeners for the given App.
   *
   * @param app the App instance to register listeners for
   * @param listeners the pre-discovered listeners
   */
  static void registerListeners(App app, Collection<AppLifecycleListener> listeners) {
    if (app == null || listeners == null || listeners.isEmpty()) {
      return;
    }

    appListeners.put(app, new ArrayList<>(listeners));
    logger.log(Level.INFO,
        String.format("Registered %d lifecycle listeners for %s", listeners.size(), app.getId()));
  }

  /**
   * Gets the lifecycle listeners associated with the given App instance.
   *
   * @param app the App instance
   * @return a collection of listeners, or an empty collection if none are registered
   */
  static Collection<AppLifecycleListener> getListeners(App app) {
    if (app == null) {
      return Collections.emptyList();
    }

    return appListeners.getOrDefault(app, Collections.emptyList());
  }

  /**
   * Removes all listeners associated with the given App instance.
   *
   * <p>
   * This method should be called when an App is terminated to prevent memory leaks.
   * </p>
   *
   * @param app the App instance to unregister listeners for
   */
  static void unregisterListeners(App app) {
    List<AppLifecycleListener> removed = appListeners.remove(app);
    if (removed != null && !removed.isEmpty()) {
      logger.log(Level.INFO,
          String.format("Unregistered %d lifecycle listeners for %s", removed.size(), app.getId()));
    }
  }

  /**
   * Discovers lifecycle listeners using ServiceLoader.
   *
   * @return a collection of discovered and sorted listeners
   * @since 25.03
   */
  static Collection<AppLifecycleListener> discoverListeners() {
    List<ListenerEntry> entries = new ArrayList<>();

    // Use ServiceLoader to discover listener implementations
    ServiceLoader<AppLifecycleListener> loader = ServiceLoader.load(AppLifecycleListener.class);

    for (AppLifecycleListener listener : loader) {
      // Check for priority annotation
      int priority = 10; // default priority
      AppListenerPriority priorityAnnotation =
          listener.getClass().getAnnotation(AppListenerPriority.class);
      if (priorityAnnotation != null) {
        priority = priorityAnnotation.value();
      }

      entries.add(new ListenerEntry(listener, priority));
      logger.log(Level.DEBUG, String.format("Discovered lifecycle listener: %s [Priority: %d]",
          listener.getClass().getName(), priority));
    }

    // Sort by priority (lower values first)
    entries.sort(Comparator.comparingInt(e -> e.priority));

    // Extract sorted listeners
    List<AppLifecycleListener> sortedListeners = new ArrayList<>();
    for (ListenerEntry entry : entries) {
      sortedListeners.add(entry.listener);
    }

    logger.log(Level.INFO,
        String.format("Discovered %d lifecycle listeners", sortedListeners.size()));

    return Collections.unmodifiableList(sortedListeners);
  }


  /**
   * Notifies all listeners with the given action, handling exceptions.
   *
   * @param listeners the collection of listeners to notify
   * @param action the action to perform on each listener
   * @param phase description of the current phase for error logging
   * @since 25.03
   */
  static void notifyListeners(Collection<AppLifecycleListener> listeners,
      Consumer<AppLifecycleListener> action, String phase) {
    for (AppLifecycleListener listener : listeners) {
      try {
        action.accept(listener);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            "Error in listener " + listener.getClass().getName() + " during " + phase, e);
      }
    }
  }

  /**
   * Hold listener with priority for sorting.
   */
  private static class ListenerEntry {
    final AppLifecycleListener listener;
    final int priority;

    ListenerEntry(AppLifecycleListener listener, int priority) {
      this.listener = listener;
      this.priority = priority;
    }
  }
}
