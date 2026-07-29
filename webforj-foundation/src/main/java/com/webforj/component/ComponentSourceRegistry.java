package com.webforj.component;

import com.webforj.environment.ObjectTable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Registry that tracks where components are instantiated in source code.
 *
 * @author Hyyan Abo Fakher
 * @since 25.12
 */
public final class ComponentSourceRegistry {

  private static final String STORAGE_KEY = ComponentSourceRegistry.class.getName();

  /**
   * The maximum number of frames returned by {@link #getSourceChain(Object)}.
   */
  private static final int MAX_CHAIN_SIZE = 10;

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
    List<SourcePoint> chain = getSourceChain(component);
    if (chain.isEmpty()) {

      return null;
    }

    return chain.get(0);
  }

  /**
   * Finds the full chain of source points leading to where a component was instantiated.
   *
   * <p>
   * The first entry is the creation site (the frame closest to where the component was constructed)
   * and subsequent entries are the callers up the stack, in stack order. Frames belonging to
   * framework packages are filtered out.
   * </p>
   *
   * @param component the component
   * @return the list of source points, or an empty list if the component is not registered
   */
  public static List<SourcePoint> getSourceChain(Object component) {
    Map<Integer, Throwable> storage = getStorage();
    Throwable t = storage.get(System.identityHashCode(component));
    if (t == null) {
      return List.of();
    }

    List<SourcePoint> chain = new ArrayList<>();
    for (StackTraceElement frame : t.getStackTrace()) {
      if (chain.size() >= MAX_CHAIN_SIZE) {
        break;
      }

      String className = frame.getClassName();
      if (className.startsWith("com.webforj.component.") || className.startsWith("com.basis.")
          || className.startsWith("java.") || className.startsWith("jdk.")
          || className.startsWith("sun.")) {
        continue;
      }

      chain.add(new SourcePoint(className, frame.getFileName(), frame.getLineNumber()));
    }

    return chain;
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
