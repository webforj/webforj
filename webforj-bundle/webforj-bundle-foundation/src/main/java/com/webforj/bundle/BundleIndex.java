package com.webforj.bundle;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Immutable index of the bundle outputs produced for the application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleIndex {

  /** Reserved binding key carrying the single eager bundle a runtime loads at app start. */
  public static final String EAGER_KEY = "eager";

  /** Reserved binding key carrying the output files a runtime injects only in debug mode. */
  public static final String DEBUG_KEY = "debug";

  /** Reserved binding key carrying the output files a runtime injects once for every view. */
  public static final String GLOBAL_KEY = "global";

  private final Map<String, List<String>> bindings;
  private final List<String> eagerFiles;
  private final List<String> debugFiles;
  private final List<String> globalFiles;

  /**
   * Creates an index.
   *
   * @param bindings an ordered map of routed class name to its list of output files, optionally
   *        carrying the reserved {@value #EAGER_KEY} entry for an eager bundle, the reserved
   *        {@value #DEBUG_KEY} entry for the output files injected only in debug mode, and the
   *        reserved {@value #GLOBAL_KEY} entry for the output files injected once for every view
   */
  public BundleIndex(Map<String, List<String>> bindings) {
    Map<String, List<String>> copy = new LinkedHashMap<>();
    List<String> eager = List.of();
    List<String> debug = List.of();
    List<String> global = List.of();

    for (Map.Entry<String, List<String>> entry : bindings.entrySet()) {
      if (EAGER_KEY.equals(entry.getKey())) {
        eager = List.copyOf(entry.getValue());
      } else if (DEBUG_KEY.equals(entry.getKey())) {
        debug = List.copyOf(entry.getValue());
      } else if (GLOBAL_KEY.equals(entry.getKey())) {
        global = List.copyOf(entry.getValue());
      } else {
        copy.put(entry.getKey(), List.copyOf(entry.getValue()));
      }
    }

    this.bindings = Collections.unmodifiableMap(copy);
    this.eagerFiles = eager;
    this.debugFiles = debug;
    this.globalFiles = global;
  }

  /**
   * Gets the routed class name to output files mapping.
   *
   * @return the bindings
   */
  public Map<String, List<String>> getBindings() {
    return bindings;
  }

  /**
   * Gets the single eager bundle output files, empty when the build produces per component output.
   *
   * @return the eager bundle output files, never null
   */
  public List<String> getEagerFiles() {
    return eagerFiles;
  }

  /**
   * Gets the output files a runtime injects only in debug mode.
   *
   * @return the debug only output files, never null
   */
  public List<String> getDebugFiles() {
    return debugFiles;
  }

  /**
   * Gets the output files a runtime injects once for every view, independent of the routed class.
   *
   * @return the global output files, never null
   */
  public List<String> getGlobalFiles() {
    return globalFiles;
  }
}
