package org.hotswap.agent.config;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.hotswap.agent.watch.Watcher;

/**
 * Test support type standing in for the plugin manager of the hotswap tool.
 */
public class PluginManager {

  private static final List<Map.Entry<String, ClassLoader>> CALLS = new ArrayList<>();
  private static final PluginManager INSTANCE = new PluginManager();

  private final Watcher watcher =
      classLoader -> CALLS.add(Map.entry("watcher.closeClassLoader", classLoader));

  /**
   * Returns the singleton.
   *
   * @return the singleton
   */
  public static PluginManager getInstance() {
    return INSTANCE;
  }

  /**
   * Returns the recorded calls.
   *
   * @return a copy of the recorded calls
   */
  public static List<Map.Entry<String, ClassLoader>> getCalls() {
    return List.copyOf(CALLS);
  }

  /**
   * Clears the recorded calls.
   */
  public static void clearCalls() {
    CALLS.clear();
  }

  /**
   * Registers a class loader.
   *
   * @param classLoader the class loader
   */
  public void initClassLoader(ClassLoader classLoader) {
    CALLS.add(Map.entry("initClassLoader", classLoader));
  }

  /**
   * Releases a class loader.
   *
   * @param classLoader the class loader
   */
  public void closeClassLoader(ClassLoader classLoader) {
    CALLS.add(Map.entry("closeClassLoader", classLoader));
  }

  /**
   * Returns the watcher.
   *
   * @return the watcher
   */
  public Watcher getWatcher() {
    return watcher;
  }
}
