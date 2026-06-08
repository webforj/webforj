package com.webforj.bundle;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The on disk shape of a {@link BundleIndex}, mapped directly to and from the index JSON.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class BundleIndexDocument {

  /**
   * The index resource a built application emits and reads at runtime.
   */
  public static final String RESOURCE = "META-INF/webforj/frontend-entries.json";

  /**
   * The index resource the development watch emits and reads.
   *
   * <p>
   * It sits under the static folder, which the development servers exclude from their reload scan,
   * so refreshing it never redeploys the running application the way a write under {@code META-INF}
   * would.
   * </p>
   */
  public static final String DEVELOPMENT_RESOURCE = "static/webforj/frontend-entries.json";

  Map<String, List<String>> bindings;
  List<String> eager;
  List<String> debug;
  List<String> global;

  /**
   * Creates an empty document, used by the JSON reader.
   */
  public BundleIndexDocument() {}

  /**
   * Creates a document from the parts the writer holds.
   *
   * @param bindings the routed class name to output files binding
   * @param eager the single eager bundle output files, or {@code null} to omit the key
   * @param debug the debug only output files, or {@code null} to omit the key
   * @param global the output files injected once for every view, or {@code null} to omit the key
   */
  public BundleIndexDocument(Map<String, List<String>> bindings, List<String> eager,
      List<String> debug, List<String> global) {
    this.bindings = bindings;
    this.eager = eager;
    this.debug = debug;
    this.global = global;
  }

  /**
   * Converts this document into the validated immutable index.
   *
   * <p>
   * Entries are trimmed and empty paths dropped.
   * </p>
   *
   * @return the index
   */
  public BundleIndex toIndex() {
    Map<String, List<String>> merged = new LinkedHashMap<>();
    if (bindings != null) {
      for (Map.Entry<String, List<String>> entry : bindings.entrySet()) {
        merged.put(entry.getKey(), sanitize(entry.getValue()));
      }
    }

    List<String> eagerFiles = sanitize(eager);
    if (!eagerFiles.isEmpty()) {
      merged.put(BundleIndex.EAGER_KEY, eagerFiles);
    }

    List<String> debugFiles = sanitize(debug);
    if (!debugFiles.isEmpty()) {
      merged.put(BundleIndex.DEBUG_KEY, debugFiles);
    }

    List<String> globalFiles = sanitize(global);
    if (!globalFiles.isEmpty()) {
      merged.put(BundleIndex.GLOBAL_KEY, globalFiles);
    }

    return new BundleIndex(merged);
  }

  private static List<String> sanitize(List<String> values) {
    if (values == null) {
      return List.of();
    }

    List<String> files = new ArrayList<>();
    for (String value : values) {
      if (value != null && !value.trim().isEmpty()) {
        files.add(value.trim());
      }
    }

    return List.copyOf(files);
  }
}
