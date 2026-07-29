package com.webforj.devtools.craftforj.inspector.source.resolver;

import com.webforj.environment.ObjectTable;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

/**
 * Per-session record of the source paths the server has resolved.
 *
 * <p>
 * Every path the server resolves from a recorded component or route is registered here. Actions
 * that accept a file path from the client honor it only when it was previously resolved
 * server-side, which confines reads and writes to real component sources. Outside a webforJ session
 * the registry is empty, so validation fails closed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class SourcePathRegistry {

  private static final String STORAGE_KEY = SourcePathRegistry.class.getName();

  private SourcePathRegistry() {}

  /**
   * Records a server-resolved source path.
   *
   * @param path the absolute source file path, ignored when {@code null}
   */
  public static void addPath(String path) {
    if (path == null) {
      return;
    }

    getStorage().add(normalize(path));
  }

  /**
   * Checks whether the path was previously resolved server-side.
   *
   * @param path the path to check, may be {@code null}
   * @return {@code true} when the path is a recorded source path
   */
  public static boolean isRecorded(String path) {
    if (path == null) {
      return false;
    }

    return getStorage().contains(normalize(path));
  }

  private static String normalize(String path) {
    return Path.of(path).toAbsolutePath().normalize().toString();
  }

  @SuppressWarnings("unchecked")
  private static Set<String> getStorage() {
    try {
      if (!ObjectTable.contains(STORAGE_KEY)) {
        ObjectTable.put(STORAGE_KEY, new HashSet<String>());
      }

      return (Set<String>) ObjectTable.get(STORAGE_KEY);
    } catch (Exception e) {
      return new HashSet<>();
    }
  }
}
