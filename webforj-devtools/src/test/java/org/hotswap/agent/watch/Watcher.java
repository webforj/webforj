package org.hotswap.agent.watch;

/**
 * Test support type standing in for the watcher of the hotswap tool.
 */
public interface Watcher {

  /**
   * Releases a class loader.
   *
   * @param classLoader the class loader
   */
  void closeClassLoader(ClassLoader classLoader);
}
