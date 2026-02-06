package com.webforj.component;

import com.webforj.environment.ObjectTable;
import java.util.HashMap;
import java.util.Map;

/**
 * Registry that tracks where components are instantiated in source code.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public final class ComponentSourceRegistry {

  private static final String STORAGE_KEY = "webforj.component.sources";

  /**
   * Represents where a component was instantiated.
   *
   * @param className the fully qualified class name
   * @param fileName the source file name (e.g., "MyView.java")
   * @param lineNumber the line number in the source file
   */
  public record SourcePoint(String className, String fileName, int lineNumber) {}

  private ComponentSourceRegistry() {
    // prevent instantiation
  }

  /**
   * Records where a component is being instantiated. Call from Component constructor.
   *
   * @param component the component being created
   */
  public static void register(Object component) {
    Map<Integer, Throwable> storage = getStorage();
    storage.put(System.identityHashCode(component), new Throwable());
  }

  /**
   * Finds the source point where a component was instantiated.
   *
   * @param component the component
   * @return the source point, or null if not registered
   */
  public static SourcePoint getSourcePoint(Object component) {
    Map<Integer, Throwable> storage = getStorage();
    Throwable t = storage.get(System.identityHashCode(component));
    if (t == null) {
      return null;
    }

    for (StackTraceElement frame : t.getStackTrace()) {
      String className = frame.getClassName();
      if (className.startsWith("com.webforj.component.") || className.startsWith("com.basis.")
          || className.startsWith("java.") || className.startsWith("jdk.")
          || className.startsWith("sun.")) {
        continue;
      }

      return new SourcePoint(className, frame.getFileName(), frame.getLineNumber());
    }

    return null;
  }

  @SuppressWarnings("unchecked")
  private static Map<Integer, Throwable> getStorage() {
    try {
      if (!ObjectTable.contains(STORAGE_KEY)) {
        ObjectTable.put(STORAGE_KEY, new HashMap<Integer, Throwable>());
      }
      return (Map<Integer, Throwable>) ObjectTable.get(STORAGE_KEY);
    } catch (Exception e) {
      return new HashMap<>();
    }
  }
}
