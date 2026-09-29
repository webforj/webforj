package com.webforj.component;

import com.webforj.environment.ObjectTable;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

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
    List<SourceFrame> chain = StackWalker.getInstance().walk(ComponentSourceRegistry::createChain);
    Storage storage = getStorage();
    synchronized (storage) {
      storage.put(component, chain);
    }
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
    List<SourcePoint> chain = new ArrayList<>();
    getSourceFrames(component).forEach(frame -> chain.add(frame.getSourcePoint()));

    return chain;
  }

  /**
   * Finds the frames of the stack a component was instantiated on.
   *
   * <p>
   * The frames are the ones {@link #getSourceChain(Object)} returns the source points of, in the
   * same order. A frame also names the method that was running and the instruction it ran.
   * </p>
   *
   * @param component the component
   * @return the list of frames, or an empty list if the component is not registered
   */
  public static List<SourceFrame> getSourceFrames(Object component) {
    Storage storage = getStorage();
    synchronized (storage) {
      return storage.get(component);
    }
  }

  /**
   * Gets the time a component was instantiated at.
   *
   * @param component the component
   * @return the time in milliseconds since the epoch, or {@code -1} if the component is not
   *         registered
   */
  public static long getCreationTime(Object component) {
    Storage storage = getStorage();
    synchronized (storage) {
      return storage.getTime(component);
    }
  }

  private static List<SourceFrame> createChain(Stream<StackWalker.StackFrame> frames) {
    return frames.filter(frame -> !isFrameworkClass(frame.getClassName())).limit(MAX_CHAIN_SIZE)
        .map(frame -> SourceFrame.builder()
            .setSourcePoint(
                new SourcePoint(frame.getClassName(), frame.getFileName(), frame.getLineNumber()))
            .setMethod(frame.getMethodName(), frame.getDescriptor())
            .setBytecodeIndex(frame.getByteCodeIndex()).build())
        .toList();
  }

  private static boolean isFrameworkClass(String className) {
    return className.startsWith("com.webforj.component.") || className.startsWith("com.basis.")
        || className.startsWith("java.") || className.startsWith("jdk.")
        || className.startsWith("sun.");
  }

  private static Storage getStorage() {
    try {
      if (!ObjectTable.contains(STORAGE_KEY)) {
        ObjectTable.put(STORAGE_KEY, new Storage());
      }

      return (Storage) ObjectTable.get(STORAGE_KEY);
    } catch (Exception e) {
      return new Storage();
    }
  }

  /**
   * One frame of the stack a component was instantiated on.
   */
  public static final class SourceFrame {

    private final SourcePoint sourcePoint;
    private final String methodName;
    private final String descriptor;
    private final int bytecodeIndex;

    private SourceFrame(Builder builder) {
      this.sourcePoint = builder.sourcePoint;
      this.methodName = builder.methodName;
      this.descriptor = builder.descriptor;
      this.bytecodeIndex = builder.bytecodeIndex;
    }

    /**
     * Creates a builder.
     *
     * @return the builder
     */
    public static Builder builder() {
      return new Builder();
    }

    /**
     * Gets the source point of the frame.
     *
     * @return the class, the file and the line of the frame
     */
    public SourcePoint getSourcePoint() {
      return sourcePoint;
    }

    /**
     * Gets the name of the method that was running.
     *
     * @return the method name, {@code <init>} for a constructor
     */
    public String getMethodName() {
      return methodName;
    }

    /**
     * Gets the descriptor of the method that was running.
     *
     * @return the method descriptor
     */
    public String getDescriptor() {
      return descriptor;
    }

    /**
     * Gets the index of the instruction the method ran.
     *
     * @return the index inside the code of the method, or a negative number when it is not known
     */
    public int getBytecodeIndex() {
      return bytecodeIndex;
    }

    /**
     * Builds a {@link SourceFrame}.
     */
    public static final class Builder {

      private SourcePoint sourcePoint;
      private String methodName;
      private String descriptor;
      private int bytecodeIndex = -1;

      private Builder() {}

      /**
       * Sets the source point of the frame.
       *
       * @param sourcePoint the class, the file and the line of the frame
       * @return this builder
       */
      public Builder setSourcePoint(SourcePoint sourcePoint) {
        this.sourcePoint = sourcePoint;

        return this;
      }

      /**
       * Sets the method that was running.
       *
       * @param methodName the method name
       * @param descriptor the method descriptor
       * @return this builder
       */
      public Builder setMethod(String methodName, String descriptor) {
        this.methodName = methodName;
        this.descriptor = descriptor;

        return this;
      }

      /**
       * Sets the index of the instruction the method ran.
       *
       * @param bytecodeIndex the index inside the code of the method
       * @return this builder
       */
      public Builder setBytecodeIndex(int bytecodeIndex) {
        this.bytecodeIndex = bytecodeIndex;

        return this;
      }

      /**
       * Builds the frame.
       *
       * @return the frame
       */
      public SourceFrame build() {
        return new SourceFrame(this);
      }
    }
  }

  /**
   * The chains of the components that are still alive, keyed by the component instance.
   */
  private static final class Storage {

    private final Map<Key, Entry> entries = new HashMap<>();
    private final ReferenceQueue<Object> collected = new ReferenceQueue<>();

    void put(Object component, List<SourceFrame> chain) {
      // The chain of a component that was garbage collected can never be asked for again
      for (Object key = collected.poll(); key != null; key = collected.poll()) {
        entries.remove(key);
      }

      entries.put(new Key(component, collected), new Entry(chain, System.currentTimeMillis()));
    }

    List<SourceFrame> get(Object component) {
      Entry entry = entries.get(new Key(component, null));

      return entry == null ? List.of() : entry.chain;
    }

    long getTime(Object component) {
      Entry entry = entries.get(new Key(component, null));

      return entry == null ? -1 : entry.time;
    }
  }

  /**
   * What is known about the creation of one component.
   */
  private static final class Entry {

    private final List<SourceFrame> chain;
    private final long time;

    Entry(List<SourceFrame> chain, long time) {
      this.chain = chain;
      this.time = time;
    }
  }

  /**
   * A weak key that stands for one instance, two components that are equal stay two keys.
   */
  private static final class Key extends WeakReference<Object> {

    private final int hash;

    Key(Object component, ReferenceQueue<Object> queue) {
      super(component, queue);
      this.hash = System.identityHashCode(component);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
      return hash;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object other) {
      Object component = get();

      return component != null && other instanceof Key key && key.get() == component;
    }
  }
}
