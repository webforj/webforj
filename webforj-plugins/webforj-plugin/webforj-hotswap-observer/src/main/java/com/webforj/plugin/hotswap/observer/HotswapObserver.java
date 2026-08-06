package com.webforj.plugin.hotswap.observer;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.Instrumentation;
import java.security.ProtectionDomain;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Reports every class redefinition of the application virtual machine to the live reload receiver
 * of the running application.
 *
 * <p>
 * The observer attaches as its own java agent next to the hotswap tool and listens to the virtual
 * machine instead of the tool. Whenever anything redefines a class, the virtual machine hands the
 * incoming bytes to every registered transformer, this one included, so the observer sees each
 * redefinition without touching any interface of the tool performing it. The observer never changes
 * the bytes and never calls into the virtual machine, so it can never disturb the redefinition
 * machinery it watches.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapObserver implements ClassFileTransformer {

  static final String RECEIVER_CLASS_NAME =
      "com.webforj.devtools.livereload.receiver.HotswapAgentReceiver";
  static final String RECEIVER_METHOD_NAME = "onClassRedefinition";
  static final String THREAD_NAME = "webforj-hotswap-observer";

  private final ExecutorService reporter;

  HotswapObserver() {
    this.reporter = Executors.newSingleThreadExecutor(task -> {
      Thread thread = new Thread(task, THREAD_NAME);
      thread.setDaemon(true);
      return thread;
    });
  }

  /**
   * Registers the observer in the starting virtual machine.
   *
   * @param agentArguments the arguments of the java agent, unused
   * @param instrumentation the instrumentation of the virtual machine
   */
  @SuppressWarnings("java:S1172")
  public static void premain(String agentArguments, Instrumentation instrumentation) {
    instrumentation.addTransformer(new HotswapObserver());
  }

  /**
   * {@inheritDoc}
   */
  @Override
  @SuppressWarnings("java:S1168")
  public byte[] transform(Module module, ClassLoader loader, String className,
      Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) {
    if (classBeingRedefined == null || loader == null || className == null) {
      // A first load carries no redefined class and nothing to report.
      return null;
    }

    byte[] observed = classfileBuffer.clone();
    reporter.execute(() -> report(loader, className, observed));
    return null;
  }

  private static void report(ClassLoader loader, String className, byte[] classBytes) {
    try {
      Class<?> receiver = Class.forName(RECEIVER_CLASS_NAME, true, loader);
      receiver.getMethod(RECEIVER_METHOD_NAME, String.class, byte[].class).invoke(null,
          className.replace('/', '.'), classBytes);
    } catch (ReflectiveOperationException | LinkageError e) {
      // The application does not ship the receiver, so there is nothing to notify.
    }
  }
}
