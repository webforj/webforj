package com.webforj.devtools.livereload;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Registers a class reload listener with the JRebel agent and pushes a full page reload through the
 * {@link LiveReloadServer} it is given.
 *
 * <p>
 * The JRebel agent hot swaps Java bytecode in place and never restarts the server, so none of the
 * webforJ live reload triggers that depend on a restart ever fire for a Java change. This receiver
 * closes that gap from inside the application virtual machine. When the agent is present, its
 * software development kit classes are visible to the application classloader, so this receiver
 * probes for them with reflection and registers a listener without a compile time dependency on the
 * software development kit and without any new configuration. When the agent is absent the probe
 * fails and the receiver stays inert.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class JRebelReceiver {

  private static final System.Logger logger = System.getLogger(JRebelReceiver.class.getName());
  static final String RELOADER_FACTORY_CLASS_NAME = "org.zeroturnaround.javarebel.ReloaderFactory";
  static final String CLASS_EVENT_LISTENER_CLASS_NAME =
      "org.zeroturnaround.javarebel.ClassEventListener";
  private static final long DEBOUNCE_DELAY_MILLIS = 150;

  /**
   * The agent event type reporting that a class was loaded for the first time, which is not a hot
   * swap and must never reach the browser.
   */
  private static final int EVENT_LOADED = 0;

  private final LiveReloadServer server;
  private final String reloaderFactoryClassName;
  private final String classEventListenerClassName;
  private final AtomicBoolean running = new AtomicBoolean(false);
  private final AtomicReference<ScheduledFuture<?>> pendingReload = new AtomicReference<>();

  private ScheduledExecutorService executor;
  private Object reloader;
  private Object listener;

  /**
   * Creates a receiver that pushes JRebel class reload events through the given reload server.
   *
   * @param server the reload server the reload is pushed through
   */
  public JRebelReceiver(LiveReloadServer server) {
    this(server, RELOADER_FACTORY_CLASS_NAME, CLASS_EVENT_LISTENER_CLASS_NAME);
  }

  /**
   * Creates a receiver that discovers the agent under the given type names, so a test can supply
   * its own stand in types instead of the agent ones.
   *
   * @param server the reload server the reload is pushed through
   * @param reloaderFactoryClassName the binary name of the reloader factory type
   * @param classEventListenerClassName the binary name of the class event listener interface
   */
  JRebelReceiver(LiveReloadServer server, String reloaderFactoryClassName,
      String classEventListenerClassName) {
    this.server = server;
    this.reloaderFactoryClassName = reloaderFactoryClassName;
    this.classEventListenerClassName = classEventListenerClassName;
  }

  /**
   * Probes for the JRebel agent and registers a class reload listener, unless the receiver is
   * already running or the agent is absent.
   */
  public void start() {
    if (!running.compareAndSet(false, true)) {
      return;
    }

    ClassLoader classLoader = resolveClassLoader();
    ScheduledExecutorService newExecutor = null;

    try {
      Class<?> reloaderFactoryClass = Class.forName(reloaderFactoryClassName, false, classLoader);
      Class<?> classEventListenerClass =
          Class.forName(classEventListenerClassName, true, classLoader);

      Object reloaderInstance = reloaderFactoryClass.getMethod("getInstance").invoke(null);
      Object listenerProxy = Proxy.newProxyInstance(classLoader,
          new Class<?>[] {classEventListenerClass}, this::handleInvocation);

      // everything that can fail runs before the registration, so a registration that succeeds is
      // always followed by the assignments that let stop undo it
      newExecutor = Executors.newSingleThreadScheduledExecutor(JRebelReceiver::newDaemonThread);

      reloaderInstance.getClass().getMethod("addClassReloadListener", classEventListenerClass)
          .invoke(reloaderInstance, listenerProxy);

      this.executor = newExecutor;
      this.reloader = reloaderInstance;
      this.listener = listenerProxy;

      logger.log(System.Logger.Level.INFO, "JRebel detected, registered for class reload events");
    } catch (ReflectiveOperationException | RuntimeException e) {
      if (newExecutor != null) {
        newExecutor.shutdownNow();
      }

      logger.log(System.Logger.Level.DEBUG,
          "JRebel not detected, class reload events will not trigger a browser reload", e);
      running.set(false);
    }
  }

  /**
   * Deregisters the class reload listener, cancels a pending reload, and stops the receiver.
   */
  public void stop() {
    if (!running.compareAndSet(true, false)) {
      return;
    }

    ScheduledFuture<?> scheduled = pendingReload.getAndSet(null);
    if (scheduled != null) {
      scheduled.cancel(false);
    }

    if (reloader != null && listener != null) {
      try {
        Method removeListener = findRemoveListenerMethod(reloader, listener);
        if (removeListener != null) {
          removeListener.invoke(reloader, listener);
        }
      } catch (ReflectiveOperationException | RuntimeException e) {
        logger.log(System.Logger.Level.DEBUG,
            "Could not deregister the JRebel class reload listener", e);
      }
    }

    if (executor != null) {
      executor.shutdownNow();
      executor = null;
    }

    reloader = null;
    listener = null;
  }

  /**
   * Indicates whether the receiver is running.
   *
   * @return {@code true} between a start and a stop
   */
  public boolean isRunning() {
    return running.get();
  }

  private Object handleInvocation(Object proxy, Method method, Object[] args) {
    String name = method.getName();

    if ("onClassEvent".equals(name)) {
      if (isHotSwap(args)) {
        scheduleReloadSafely();
      }

      return null;
    }

    if ("priority".equals(name)) {
      return 0;
    }

    if ("equals".equals(name)) {
      return proxy == (args != null && args.length > 0 ? args[0] : null);
    }

    if ("hashCode".equals(name)) {
      return System.identityHashCode(proxy);
    }

    if ("toString".equals(name)) {
      return JRebelReceiver.class.getSimpleName() + "$ClassEventListener";
    }

    return null;
  }

  private static boolean isHotSwap(Object[] args) {
    if (args == null || args.length == 0 || !(args[0] instanceof Integer eventType)) {
      return false;
    }

    return eventType != EVENT_LOADED;
  }

  private void scheduleReloadSafely() {
    try {
      scheduleReload();
    } catch (RuntimeException e) {
      logger.log(System.Logger.Level.DEBUG,
          "Could not schedule a browser reload for a JRebel class reload", e);
    }
  }

  private void scheduleReload() {
    ScheduledExecutorService currentExecutor = executor;
    if (currentExecutor == null) {
      return;
    }

    ScheduledFuture<?> previous = pendingReload.getAndSet(
        currentExecutor.schedule(this::sendReload, DEBOUNCE_DELAY_MILLIS, TimeUnit.MILLISECONDS));

    if (previous != null) {
      previous.cancel(false);
    }
  }

  private void sendReload() {
    pendingReload.set(null);

    if (server == null || !server.isRunning()) {
      return;
    }

    logger.log(System.Logger.Level.INFO, "Triggering browser reload for a JRebel class reload");
    server.sendReloadMessage();
  }

  private static Method findRemoveListenerMethod(Object reloader, Object listener) {
    for (Method method : reloader.getClass().getMethods()) {
      if ("removeClassReloadListener".equals(method.getName()) && method.getParameterCount() == 1
          && method.getParameterTypes()[0].isInstance(listener)) {
        return method;
      }
    }

    return null;
  }

  private static Thread newDaemonThread(Runnable task) {
    Thread thread = new Thread(task, "webforj-jrebel-receiver");
    thread.setDaemon(true);

    return thread;
  }

  private static ClassLoader resolveClassLoader() {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    return classLoader != null ? classLoader : JRebelReceiver.class.getClassLoader();
  }
}
