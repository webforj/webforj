package com.webforj.bundle.bun.writer;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.bundle.BundleIndex;
import com.webforj.bundle.BundleIndexDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Reads the Bun metafile produced by a build and writes the index file that the runtime consumes.
 *
 * <p>
 * The metafile records, for every output, the entry source it came from, so each entry path is
 * mapped to its exact output files, the entry script and any stylesheet. Shared chunks and binary
 * assets carry no entry point and are excluded, since the entry script references them relatively.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleIndexWriter {

  private static final Gson GSON = new Gson();

  /**
   * Maps each entry path to its output files by reading the Bun metafile.
   *
   * @param metafile the Bun metafile written by the build
   * @param sourceRoot the bundle source root, used to normalize entry point paths
   *
   * @return the mapping of entry path to its injectable output paths, relative to the output
   *         directory
   * @throws IOException if the metafile cannot be read
   */
  public Map<String, List<String>> mapOutputs(Path metafile, Path sourceRoot) throws IOException {
    if (metafile == null || !Files.isRegularFile(metafile)) {
      return Map.of();
    }

    JsonObject root =
        GSON.fromJson(Files.readString(metafile, StandardCharsets.UTF_8), JsonObject.class);
    JsonObject outputs = root == null ? null : root.getAsJsonObject("outputs");
    if (outputs == null) {
      return Map.of();
    }

    Map<String, List<String>> resolved = new LinkedHashMap<>();
    for (Map.Entry<String, JsonElement> output : outputs.entrySet()) {
      JsonObject info = output.getValue().getAsJsonObject();
      JsonElement entryPoint = info.get("entryPoint");

      if (entryPoint == null || entryPoint.isJsonNull()) {
        continue;
      }

      String entry = normalizeEntry(entryPoint.getAsString(), sourceRoot);
      String file = stripLeading(output.getKey());
      resolved.computeIfAbsent(entry, key -> new ArrayList<>()).add(file);
    }

    for (List<String> files : resolved.values()) {
      files.sort(Comparator.naturalOrder());
    }

    return resolved;
  }

  /**
   * Folds the entry key to output files mapping and the class to entry keys binding into the class
   * to output files binding the runtime consumes.
   *
   * <p>
   * The persisted index binds each routed class directly to the output files it loads.
   * </p>
   *
   * @param keyToFiles the entry key to output files mapping from {@link #mapOutputs(Path, Path)}
   * @param classToKeys the routed class name to declared entry keys binding
   *
   * @return the routed class name to output files binding, in class declaration order
   */
  public Map<String, List<String>> bindClasses(Map<String, List<String>> keyToFiles,
      Map<String, Set<String>> classToKeys) {
    Map<String, List<String>> classToFiles = new LinkedHashMap<>();

    for (Map.Entry<String, Set<String>> binding : classToKeys.entrySet()) {
      List<String> files = new ArrayList<>();
      for (String key : binding.getValue()) {
        List<String> outputs = keyToFiles.get(key);
        if (outputs != null) {
          files.addAll(outputs);
        }
      }

      if (!files.isEmpty()) {
        classToFiles.put(binding.getKey(), List.copyOf(files));
      }
    }

    return classToFiles;
  }

  /**
   * Writes the single index file next to the consuming module's compiled classes.
   *
   * @param classesDir the project's {@code target/classes} folder
   * @param bindings the routed class name to output files binding
   * @return the path of the file written
   * @throws IOException if writing fails
   */
  public Path write(Path classesDir, Map<String, List<String>> bindings) throws IOException {
    return write(classesDir, bindings, BundleIndexDocument.RESOURCE);
  }

  /**
   * Writes the index file to the given resource path under the consuming module's compiled classes.
   *
   * @param classesDir the project's {@code target/classes} folder
   * @param bindings the routed class name to output files binding
   * @param resource the index resource path to write, relative to {@code classesDir}
   * @return the path of the file written
   * @throws IOException if writing fails
   */
  public Path write(Path classesDir, Map<String, List<String>> bindings, String resource)
      throws IOException {
    Path target = classesDir.resolve(resource);
    Files.createDirectories(target.getParent());
    writeIfChanged(target, GSON.toJson(toDocument(bindings)));

    return target;
  }

  /**
   * Writes the content only when it differs from the file already on disk.
   *
   * <p>
   * An unchanged rebuild leaves the file timestamp untouched, so a development class path watcher
   * never sees a modification and a restart loop never starts.
   * </p>
   *
   * @param target the file to write
   * @param content the content to write
   * @throws IOException if reading the existing file or writing fails
   */
  private static void writeIfChanged(Path target, String content) throws IOException {
    if (Files.exists(target) && content.equals(Files.readString(target, StandardCharsets.UTF_8))) {
      return;
    }

    Files.writeString(target, content, StandardCharsets.UTF_8);
  }

  private static BundleIndexDocument toDocument(Map<String, List<String>> bindings) {
    Map<String, List<String>> declared = new LinkedHashMap<>();
    List<String> eager = null;
    List<String> debug = null;
    List<String> global = null;

    for (Map.Entry<String, List<String>> binding : bindings.entrySet()) {
      if (BundleIndex.EAGER_KEY.equals(binding.getKey())) {
        eager = binding.getValue();
      } else if (BundleIndex.DEBUG_KEY.equals(binding.getKey())) {
        debug = binding.getValue();
      } else if (BundleIndex.GLOBAL_KEY.equals(binding.getKey())) {
        global = binding.getValue();
      } else {
        declared.put(binding.getKey(), binding.getValue());
      }
    }

    return new BundleIndexDocument(declared, emptyToNull(eager), emptyToNull(debug),
        emptyToNull(global));
  }

  private static List<String> emptyToNull(List<String> values) {
    return values == null || values.isEmpty() ? null : values;
  }

  private static String normalizeEntry(String entryPoint, Path sourceRoot) {
    String value = stripLeading(entryPoint);
    Path path = Path.of(value);

    if (path.isAbsolute() && sourceRoot != null) {
      value = sourceRoot.toAbsolutePath().normalize().relativize(path.toAbsolutePath().normalize())
          .toString();
    }

    return value.replace('\\', '/');
  }

  private static String stripLeading(String path) {
    String value = path;
    while (value.startsWith("./")) {
      value = value.substring(2);
    }

    return value;
  }
}
