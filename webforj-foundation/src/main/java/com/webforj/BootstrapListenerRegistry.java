package com.webforj;

import com.webforj.annotation.BootstrapListenerPriority;
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

/**
 * Registry for Bootstrap lifecycle listeners using Java ServiceLoader mechanism.
 *
 * <p>
 * This class manages the discovery and lifecycle of {@link BootstrapListener} implementations using
 * the standard Java {@link ServiceLoader} mechanism. Each Bootstrap instance gets its own set of
 * listener instances to ensure proper isolation between different bootstrap operations.
 * </p>
 *
 * <p>
 * To register a listener, implementations must:
 * </p>
 * <ol>
 * <li>Implement the {@link BootstrapListener} interface</li>
 * <li>Be listed in {@code META-INF/services/com.webforj.BootstrapListener}</li>
 * <li>Optionally use {@link BootstrapListenerPriority} annotation to control execution order</li>
 * </ol>
 *
 * <h3>Exception Handling</h3>
 * <p>
 * Exceptions thrown by listeners are handled differently compared to the main bootstrap flow:
 * </p>
 * <ul>
 * <li><strong>Listener exceptions are isolated:</strong> If a listener throws an exception, it is
 * logged at ERROR level but does not stop the bootstrap process or prevent other listeners from
 * being notified.</li>
 * <li><strong>All listeners are notified:</strong> Even if one listener fails, all other registered
 * listeners will still receive their notifications.</li>
 * <li><strong>Original exceptions are preserved:</strong> Listener exceptions never replace or mask
 * the original bootstrap exceptions.</li>
 * </ul>
 *
 * @author Hyyan Abo Fakher
 * @since 25.03
 */
final class BootstrapListenerRegistry {
  private static final Logger logger = System.getLogger(BootstrapListenerRegistry.class.getName());
  private static final int DEFAULT_PRIORITY = 10;
  private static final Map<String, List<BootstrapListener>> bootstrapListeners =
      new ConcurrentHashMap<>();

  private BootstrapListenerRegistry() {
    // Prevent instantiation
  }

  /**
   * Discovers and registers lifecycle listeners for the given Bootstrap.
   *
   * <p>
   * This method uses the {@link ServiceLoader} mechanism to discover implementations of
   * {@link BootstrapListener}, instantiates them, and associates them with the given Bootstrap
   * context.
   * </p>
   *
   * @param context the bootstrap context
   */
  static void registerListeners(BootstrapContext context) {
    if (context == null) {
      return;
    }

    List<BootstrapListener> sortedListeners = discoverAndSortListeners();

    if (!sortedListeners.isEmpty()) {
      bootstrapListeners.put(context.getId(), sortedListeners);
      logger.log(Level.INFO, String.format("[%s] Registered %s bootstrap listeners",
          context.getId(), sortedListeners.size()));
    }
  }

  /**
   * Gets the lifecycle listeners associated with the given Bootstrap context.
   *
   * @param context the bootstrap context
   * @return a collection of listeners, or an empty collection if none are registered
   */
  static Collection<BootstrapListener> getListeners(BootstrapContext context) {
    if (context == null) {
      return Collections.emptyList();
    }

    return bootstrapListeners.getOrDefault(context.getId(), Collections.emptyList());
  }

  /**
   * Removes all listeners associated with the given Bootstrap context.
   *
   * <p>
   * This method should be called when a Bootstrap is completed to prevent memory leaks.
   * </p>
   *
   * @param context the bootstrap context
   */
  static void unregisterListeners(BootstrapContext context) {
    if (context == null) {
      return;
    }
    List<BootstrapListener> removed = bootstrapListeners.remove(context.getId());
    if (removed != null && !removed.isEmpty()) {
      logger.log(Level.INFO, String.format("[%s] Unregistered %s bootstrap listeners",
          context.getId(), removed.size()));
    }
  }

  /**
   * Discovers and sorts bootstrap listeners.
   *
   * @return a sorted, unmodifiable list of listeners
   */
  private static List<BootstrapListener> discoverAndSortListeners() {
    List<ListenerEntry> entries = new ArrayList<>();
    ServiceLoader<BootstrapListener> loader = ServiceLoader.load(BootstrapListener.class);

    for (BootstrapListener listener : loader) {
      // Check for priority annotation
      int priority = DEFAULT_PRIORITY;
      BootstrapListenerPriority priorityAnnotation =
          listener.getClass().getAnnotation(BootstrapListenerPriority.class);
      if (priorityAnnotation != null) {
        priority = priorityAnnotation.value();
      }

      entries.add(new ListenerEntry(listener, priority));
      logger.log(Level.DEBUG, String.format("Discovered bootstrap listener: %s [Priority: %s]",
          listener.getClass().getName(), priority));
    }

    // Sort by priority (lower values first)
    entries.sort(Comparator.comparingInt(e -> e.priority));

    // Extract sorted listeners
    List<BootstrapListener> sortedListeners = new ArrayList<>();
    for (ListenerEntry entry : entries) {
      sortedListeners.add(entry.listener);
    }

    logger.log(Level.INFO, String.format("Loaded %s bootstrap listeners", sortedListeners.size()));

    return Collections.unmodifiableList(sortedListeners);
  }

  /**
   * Notifies all listeners of the onStarting event.
   *
   * @param context the bootstrap context
   */
  static void notifyStarting(BootstrapContext context) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onStarting(context);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            "Error in bootstrap listener " + listener.getClass().getName() + " during onStarting",
            e);
      }
    }
  }

  /**
   * Notifies all listeners of the onEnvironmentPrepared event.
   *
   * @param context the bootstrap context
   */
  static void notifyEnvironmentPrepared(BootstrapContext context) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onEnvironmentPrepared(context);
      } catch (Exception e) {
        logger.log(Level.ERROR, "Error in bootstrap listener " + listener.getClass().getName()
            + " during onEnvironmentPrepared", e);
      }
    }
  }

  /**
   * Notifies all listeners of the onContextPrepared event.
   *
   * @param context the bootstrap context
   */
  static void notifyContextPrepared(BootstrapContext context) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onContextPrepared(context);
      } catch (Exception e) {
        logger.log(Level.ERROR, "Error in bootstrap listener " + listener.getClass().getName()
            + " during onContextPrepared", e);
      }
    }
  }

  /**
   * Notifies all listeners of the onAppCreated event.
   *
   * @param context the bootstrap context
   * @param app the created App instance
   */
  static void notifyAppCreated(BootstrapContext context, App app) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onAppCreated(context, app);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            "Error in bootstrap listener " + listener.getClass().getName() + " during onAppCreated",
            e);
      }
    }
  }

  /**
   * Notifies all listeners of the onFailed event.
   *
   * @param context the bootstrap context
   * @param exception the exception that caused the failure
   */
  static void notifyFailed(BootstrapContext context, Throwable exception) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onFailed(context, exception);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            "Error in bootstrap listener " + listener.getClass().getName() + " during onFailed", e);
      }
    }
  }

  /**
   * Notifies all listeners of the onCleanup event.
   *
   * @param context the bootstrap context
   */
  static void notifyCleanup(BootstrapContext context) {
    for (BootstrapListener listener : getListeners(context)) {
      try {
        listener.onCleanup(context);
      } catch (Exception e) {
        logger.log(Level.ERROR,
            "Error in bootstrap listener " + listener.getClass().getName() + " during onCleanup",
            e);
      }
    }
  }

  private static class ListenerEntry {
    final BootstrapListener listener;
    final int priority;

    ListenerEntry(BootstrapListener listener, int priority) {
      this.listener = listener;
      this.priority = priority;
    }
  }
}
