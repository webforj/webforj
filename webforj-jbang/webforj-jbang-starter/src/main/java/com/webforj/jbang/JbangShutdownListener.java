package com.webforj.jbang;

import com.webforj.App;
import com.webforj.AppLifecycleListener;
import com.webforj.spring.ContextHolder;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;

/**
 * Lifecycle listener that automatically shuts down the Spring Boot application when all browser
 * windows are closed.
 *
 * <p>
 * This listener is designed for JBang scripts where the application should terminate when the user
 * closes all browser windows. It tracks active {@link App} instances and initiates a graceful
 * shutdown when no apps have been running for a configurable idle period.
 * </p>
 *
 * <h2>Configuration</h2>
 * <p>
 * The listener can be configured using the following system properties:
 * </p>
 *
 * <table border="1">
 * <caption>System Properties</caption>
 * <tr>
 * <th>Property</th>
 * <th>Description</th>
 * <th>Default</th>
 * </tr>
 * <tr>
 * <td>{@code webforj.jbang.auto-shutdown}</td>
 * <td>Enable or disable automatic shutdown. Set to {@code false} to keep the server running
 * indefinitely.</td>
 * <td>{@code true}</td>
 * </tr>
 * <tr>
 * <td>{@code webforj.jbang.idle-timeout}</td>
 * <td>Number of seconds to wait with zero active apps before initiating shutdown. Increase this
 * value if page refreshes are causing premature shutdown.</td>
 * <td>{@code 5}</td>
 * </tr>
 * </table>
 *
 * <h2>Example Usage</h2>
 * <p>
 * To disable auto-shutdown:
 * </p>
 *
 * <pre>
 * {@code
 * java -Dwebforj.jbang.auto-shutdown=false MyApp.java
 * }
 * </pre>
 *
 * <p>
 * To increase the idle timeout to 10 seconds:
 * </p>
 *
 * <pre>
 * {@code
 * java -Dwebforj.jbang.idle-timeout=10 MyApp.java
 * }
 * </pre>
 *
 * @author Hyyan Abo Fakher
 * @since 25.11
 */
public class JbangShutdownListener implements AppLifecycleListener {

  private static final AtomicInteger activeApps = new AtomicInteger(0);
  private static final AtomicLong lastActiveTime = new AtomicLong(System.currentTimeMillis());
  private static volatile boolean intervalStarted = false;
  private static volatile boolean enabled = false;
  private static long idleTimeoutMillis = 5000;

  private static final ScheduledExecutorService scheduler =
      Executors.newSingleThreadScheduledExecutor(r -> {
        Thread t = new Thread(r, "jbang-shutdown-monitor");
        t.setDaemon(true);
        return t;
      });

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidRun(App app) {
    enabled = Boolean.parseBoolean(System.getProperty("webforj.jbang.auto-shutdown", "true"));

    if (!enabled) {
      return;
    }

    idleTimeoutMillis =
        Long.parseLong(System.getProperty("webforj.jbang.idle-timeout", "5")) * 1000;

    // Register this app
    activeApps.incrementAndGet();
    lastActiveTime.set(System.currentTimeMillis());

    // Start the monitor interval once
    if (!intervalStarted) {
      intervalStarted = true;
      startShutdownMonitor();
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void onDidTerminate(App app) {
    if (!enabled) {
      return;
    }

    // Unregister this app
    if (activeApps.decrementAndGet() > 0) {
      // Still have active apps, update the last active time
      lastActiveTime.set(System.currentTimeMillis());
    }
  }

  private static void startShutdownMonitor() {
    scheduler.scheduleAtFixedRate(() -> {
      int count = activeApps.get();

      if (count > 0) {
        // Apps are running, update last active time
        lastActiveTime.set(System.currentTimeMillis());
      } else {
        // No apps running, check if idle timeout exceeded
        long idleTime = System.currentTimeMillis() - lastActiveTime.get();
        if (idleTime >= idleTimeoutMillis) {
          shutdown();
        }
      }
    }, 1, 1, TimeUnit.SECONDS);
  }

  private static void shutdown() {
    ApplicationContext context = ContextHolder.getContext();
    if (context != null) {
      int exitCode = SpringApplication.exit(context, () -> 0);
      System.exit(exitCode);
    } else {
      System.exit(0);
    }
  }

  static int getActiveAppsCount() {
    return activeApps.get();
  }

  static void resetState() {
    activeApps.set(0);
    lastActiveTime.set(System.currentTimeMillis());
    enabled = false;
  }
}
