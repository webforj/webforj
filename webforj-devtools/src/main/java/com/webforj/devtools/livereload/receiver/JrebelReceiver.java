package com.webforj.devtools.livereload.receiver;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Registers a class reload listener with the JRebel agent and pushes the redefined class names
 * through the {@link LiveReloadServer} it is given, falling back to a full page reload when the
 * agent does not name the class.
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
public class JrebelReceiver implements LiveReloadReceiver {

  private static final System.Logger logger = System.getLogger(JrebelReceiver.class.getName());
  static final String RELOADER_FACTORY_CLASS_NAME = "org.zeroturnaround.javarebel.ReloaderFactory";
  static final String CLASS_EVENT_LISTENER_CLASS_NAME =
      "org.zeroturnaround.javarebel.ClassEventListener";

  /**
   * The agent event type reporting that a class was loaded for the first time, which is not a hot
   * swap and must never reach the browser.
   */
  private static final int EVENT_LOADED = 0;

  private final String reloaderFactoryClassName;
  private final String classEventListenerClassName;
  private final ClassUpdateDelivery delivery;
  private final AtomicBoolean running = new AtomicBoolean(false);

  private Object reloader;
  private Object listener;

  /**
   * Creates a receiver that pushes JRebel class reload events through the given reload server.
   *
   * @param server the reload server the reload is pushed through
   */
  public JrebelReceiver(LiveReloadServer server) {
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
  JrebelReceiver(LiveReloadServer server, String reloaderFactoryClassName,
      String classEventListenerClassName) {
    this.reloaderFactoryClassName = reloaderFactoryClassName;
    this.classEventListenerClassName = classEventListenerClassName;
    this.delivery =
        new ClassUpdateDelivery(server, "a JRebel class reload", "webforj-jrebel-receiver");
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

    try {
      Class<?> reloaderFactoryClass = Class.forName(reloaderFactoryClassName, false, classLoader);
      Class<?> classEventListenerClass =
          Class.forName(classEventListenerClassName, true, classLoader);

      Object reloaderInstance = reloaderFactoryClass.getMethod("getInstance").invoke(null);
      Object listenerProxy = Proxy.newProxyInstance(classLoader,
          new Class<?>[] {classEventListenerClass}, this::handleInvocation);

      // everything that can fail runs before the registration, so a registration that succeeds is
      // always followed by the assignments that let stop undo it
      delivery.start();

      reloaderInstance.getClass().getMethod("addClassReloadListener", classEventListenerClass)
          .invoke(reloaderInstance, listenerProxy);

      this.reloader = reloaderInstance;
      this.listener = listenerProxy;

      logger.log(System.Logger.Level.INFO, "JRebel detected, registered for class reload events");
    } catch (ReflectiveOperationException | RuntimeException e) {
      delivery.stop();

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

    delivery.stop();

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

  void sendReload() {
    delivery.deliver();
  }

  private Object handleInvocation(Object proxy, Method method, Object[] args) {
    String name = method.getName();

    if ("onClassEvent".equals(name)) {
      if (isHotSwap(args)) {
        recordChangedClass(args);
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
      return JrebelReceiver.class.getSimpleName() + "$ClassEventListener";
    }

    return null;
  }

  private static boolean isHotSwap(Object[] args) {
    if (args == null || args.length == 0 || !(args[0] instanceof Integer eventType)) {
      return false;
    }

    return eventType != EVENT_LOADED;
  }

  private void recordChangedClass(Object[] args) {
    if (args != null && args.length > 1 && args[1] instanceof Class<?> changedClass) {
      delivery.classChanged(changedClass.getName());
    } else {
      // Without the class name the change cannot be accounted for, so this batch falls back to
      // the full page reload.
      delivery.unnamedChange();
    }
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

  private static ClassLoader resolveClassLoader() {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    return classLoader != null ? classLoader : JrebelReceiver.class.getClassLoader();
  }
}
