package com.webforj.bbjship.assets;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.JarFile;

/**
 * Holds, per deployment folder, the index from a resource name to the jar that holds it.
 *
 * <p>
 * An index is built once by scanning the entries of each jar, opening and closing every jar. No jar
 * stays open between requests.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class ResourceIndexes {

  /**
   * Opens a jar.
   */
  @FunctionalInterface
  interface JarOpener {

    JarFile open(Path jar) throws IOException;
  }

  private final JarOpener opener;
  private final Map<Path, Map<String, Path>> indexes = new ConcurrentHashMap<>();

  ResourceIndexes() {
    this(jar -> new JarFile(jar.toFile()));
  }

  ResourceIndexes(JarOpener opener) {
    this.opener = opener;
  }

  /**
   * Finds the index of a deployment folder, building it from the jars when it is missing.
   *
   * @param folder the deployment folder
   * @param jars the jars of the deployment in classpath order
   *
   * @return the resource names mapped to the first jar that holds them
   * @throws IOException if a jar cannot be read
   */
  Map<String, Path> get(Path folder, List<Path> jars) throws IOException {
    Map<String, Path> index = indexes.get(folder);
    if (index != null) {
      return index;
    }

    Map<String, Path> built = build(jars);
    Map<String, Path> raced = indexes.putIfAbsent(folder, built);

    return raced == null ? built : raced;
  }

  JarFile open(Path jar) throws IOException {
    return opener.open(jar);
  }

  /**
   * Drops the index of every deployment folder that is gone.
   */
  void dropRemoved() {
    indexes.keySet().removeIf(folder -> !Files.isDirectory(folder));
  }

  int size() {
    return indexes.size();
  }

  private Map<String, Path> build(List<Path> jars) throws IOException {
    Map<String, Path> index = new HashMap<>();
    for (Path path : jars) {
      try (JarFile jar = opener.open(path)) {
        jar.stream().filter(entry -> !entry.isDirectory())
            .forEach(entry -> index.putIfAbsent(entry.getName(), path));
      }
    }

    return Map.copyOf(index);
  }
}
