package com.webforj.devtools.livereload.receiver;

import com.webforj.devtools.livereload.LiveReloadServer;
import java.lang.management.ManagementFactory;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.CRC32;

/**
 * Receives HotswapAgent class redefinitions and pushes the redefined class names through the
 * {@link LiveReloadServer} it is given, falling back to a full page reload when the redefinition
 * does not name the class.
 *
 * <p>
 * The HotswapAgent java agent redefines Java bytecode in place and never restarts the server, so
 * none of the restart signals the reload pipeline reacts to ever fire. The webforJ forwarder
 * running inside the agent reports every redefinition to this receiver instead, and a quick burst
 * of redefinitions collapses into one update.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class HotswapAgentReceiver implements LiveReloadReceiver {

  static final String JAVA_AGENT_ARGUMENT_PREFIX = "-javaagent:";
  static final String JAVA_AGENT_ARGUMENT_MARKER = "hotswap";
  static final String RUNTIME_AGENT_ARGUMENT_PREFIX = "-XX:HotswapAgent";

  private static final System.Logger logger =
      System.getLogger(HotswapAgentReceiver.class.getName());
  private static final Set<HotswapAgentReceiver> activeReceivers = ConcurrentHashMap.newKeySet();

  private final boolean agentPresent;
  private final ClassUpdateDelivery delivery;
  private final AtomicBoolean running = new AtomicBoolean(false);
  private final Map<String, Long> lastSeenDigests = new ConcurrentHashMap<>();

  /**
   * Creates a receiver that pushes class redefinition events through the given reload server.
   *
   * @param server the reload server the update is pushed through
   */
  public HotswapAgentReceiver(LiveReloadServer server) {
    this(server, ManagementFactory.getRuntimeMXBean().getInputArguments());
  }

  /**
   * Creates a receiver that detects the agent in the given virtual machine arguments, so a test can
   * supply its own arguments instead of the real ones.
   *
   * @param server the reload server the update is pushed through
   * @param vmArguments the virtual machine startup arguments the agent is detected in
   */
  HotswapAgentReceiver(LiveReloadServer server, List<String> vmArguments) {
    this.agentPresent = isAgentPresent(vmArguments);
    this.delivery = new ClassUpdateDelivery(server, "a HotswapAgent class redefinition",
        "webforj-hotswap-receiver");
  }

  /**
   * Accepts a class redefinition reported by the agent forwarder and schedules a debounced update
   * on every running receiver.
   *
   * <p>
   * The forwarder resolves this method reflectively through the application classloader and calls
   * it on the thread that redefines the class, so the method returns quickly and never throws.
   * </p>
   *
   * @param className the name of the redefined class, in the binary or the internal form
   * @param classBytes the class bytes the redefinition installs
   */
  public static void onClassRedefinition(String className, byte[] classBytes) {
    for (HotswapAgentReceiver receiver : activeReceivers) {
      receiver.scheduleUpdateSafely(className, classBytes);
    }
  }

  /**
   * Registers the receiver for class redefinition events, unless the receiver is already running or
   * the agent is absent.
   */
  public void start() {
    if (!running.compareAndSet(false, true)) {
      return;
    }

    if (!agentPresent) {
      logger.log(System.Logger.Level.DEBUG,
          "HotswapAgent not detected, class redefinitions will not trigger a browser update");
      running.set(false);
      return;
    }

    delivery.start();
    activeReceivers.add(this);

    logger.log(System.Logger.Level.INFO,
        "HotswapAgent detected, registered for class redefinition events");
  }

  /**
   * Deregisters the receiver, cancels a pending update, and stops the receiver.
   */
  public void stop() {
    if (!running.compareAndSet(true, false)) {
      return;
    }

    activeReceivers.remove(this);
    delivery.stop();
    lastSeenDigests.clear();
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

  private void scheduleUpdateSafely(String className, byte[] classBytes) {
    try {
      scheduleUpdate(className, classBytes);
    } catch (RuntimeException e) {
      logger.log(System.Logger.Level.DEBUG,
          "Could not schedule a browser update for a class redefinition", e);
    }
  }

  private void scheduleUpdate(String reportedName, byte[] classBytes) {
    if (!delivery.isRunning()) {
      return;
    }

    // The agent reports the internal form of the name, slashes between the packages, while the
    // rest of the pipeline works with binary names, so the dots enter here once.
    String className = reportedName != null ? reportedName.replace('/', '.') : null;

    // A repeated redefinition of unchanged bytes is one edit arriving again, a save writing the
    // identical file for example, and updates nothing. A genuine change carries different bytes.
    if (className != null && classBytes != null) {
      Long digest = digest(classBytes);
      Long previous = lastSeenDigests.put(className, digest);
      if (digest.equals(previous)) {
        logger.log(System.Logger.Level.DEBUG,
            "Class {0} was redefined with unchanged bytes, no update", className);
        return;
      }
    }

    if (className != null) {
      delivery.classChanged(className);
    } else {
      // Without the class name the change cannot be accounted for, so this batch falls back to
      // the full page reload.
      delivery.unnamedChange();
    }
  }

  private static boolean isAgentPresent(List<String> vmArguments) {
    for (String argument : vmArguments) {
      boolean javaAgent = argument.startsWith(JAVA_AGENT_ARGUMENT_PREFIX)
          && argument.contains(JAVA_AGENT_ARGUMENT_MARKER);

      if (javaAgent || argument.startsWith(RUNTIME_AGENT_ARGUMENT_PREFIX)) {
        return true;
      }
    }

    return false;
  }

  private static Long digest(byte[] classBytes) {
    CRC32 crc = new CRC32();
    crc.update(classBytes);

    // The length rides in the upper bits, so two classes of different sizes never share a digest
    // through a checksum collision alone.
    return (((long) classBytes.length) << 32) ^ crc.getValue();
  }
}
