package com.webforj.bundle.bun.discovery;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Resolves the declared entry paths to source files under a bundle source root.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleEntryResolver {

  /**
   * Resolves the declared entry paths against the given root.
   *
   * @param root the bundle source root, typically src/main/frontend
   * @param entryPaths the entry paths declared through {@code @BundleEntry}, relative to the root
   *
   * @return the resolved entries (never null, possibly empty)
   */
  public List<BundleEntryDeclaration> resolve(Path root, Collection<String> entryPaths) {
    return resolve(root, null, entryPaths);
  }

  /**
   * Resolves the declared entry paths against the given root, falling back to the extracted root
   * for a file a dependency shipped.
   *
   * @param root the bundle source root, typically src/main/frontend
   * @param extractedRoot the directory dependency frontend was extracted into, or null
   * @param entryPaths the entry paths declared through {@code @BundleEntry}, relative to a root
   *
   * @return the resolved entries (never null, possibly empty)
   */
  public List<BundleEntryDeclaration> resolve(Path root, Path extractedRoot,
      Collection<String> entryPaths) {
    if (root == null || !Files.isDirectory(root) || entryPaths == null || entryPaths.isEmpty()) {
      return List.of();
    }

    List<BundleEntryDeclaration> entries = new ArrayList<>();
    for (String path : entryPaths) {
      Path file = resolveFile(root, path);
      if (file == null && extractedRoot != null) {
        file = resolveFile(extractedRoot, path);
      }

      if (file != null) {
        entries.add(new BundleEntryDeclaration().setResolvedFile(file).setSource(path));
      } else if (isBareSpecifier(path)) {
        // Not a local file and not an explicit path, so treat it as an npm specifier Bun resolves
        // from node_modules. This is how a project consumes a package with no frontend source.
        entries.add(new BundleEntryDeclaration().setSource(path).setNpm(true));
      }
    }

    return List.copyOf(entries);
  }

  /**
   * Gets the entry values that look like a local path but resolve to no source file under the given
   * root.
   *
   * <p>
   * A bare npm specifier is not reported here, it is resolved by Bun at build time.
   * </p>
   *
   * @param root the bundle source root
   * @param entryPaths the entry values declared through {@code @BundleEntry}
   * @return the values with no matching source file that are not npm specifiers
   */
  public List<String> getUnresolved(Path root, Collection<String> entryPaths) {
    return getUnresolved(root, null, entryPaths);
  }

  /**
   * Gets the entry values that look like a local path but resolve to no source file under either
   * root.
   *
   * @param root the bundle source root
   * @param extractedRoot the directory dependency frontend was extracted into, or null
   * @param entryPaths the entry values declared through {@code @BundleEntry}
   * @return the values with no matching source file that are not npm specifiers
   */
  public List<String> getUnresolved(Path root, Path extractedRoot, Collection<String> entryPaths) {
    if (entryPaths == null || entryPaths.isEmpty()) {
      return List.of();
    }

    List<String> missing = new ArrayList<>();
    for (String path : entryPaths) {
      boolean resolved = (root != null && resolveFile(root, path) != null)
          || (extractedRoot != null && resolveFile(extractedRoot, path) != null);
      if (!resolved && !isBareSpecifier(path)) {
        missing.add(path);
      }
    }

    return List.copyOf(missing);
  }

  private static boolean isBareSpecifier(String value) {
    return !value.startsWith("./") && !value.startsWith("../") && !value.startsWith("/");
  }

  private Path resolveFile(Path root, String path) {
    Path candidate = root.resolve(path).normalize();
    if (candidate.startsWith(root) && Files.isRegularFile(candidate)) {
      return candidate;
    }

    return null;
  }
}
