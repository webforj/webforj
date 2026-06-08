package com.webforj.bundle.bun.discovery;

import io.github.classgraph.ClassGraph;
import io.github.classgraph.Resource;
import io.github.classgraph.ScanResult;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;

/**
 * Extracts the frontend files a dependency ships inside its JAR, under {@value #RESOURCE_ROOT},
 * into a directory the single Bun build compiles.
 *
 * <p>
 * This lets a component library deliver its frontend source as a normal Maven dependency, the way
 * an app delivers its own under {@code src/main/frontend}.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class DependencyEntryExtractor {

  /** The resource folder a dependency ships its frontend under. */
  public static final String RESOURCE_ROOT = "META-INF/webforj/frontend";

  /**
   * Copies every frontend file found under {@value #RESOURCE_ROOT} on the classpath into the target
   * directory, preserving the path below that root.
   *
   * @param classpathRoots the classpath roots to scan (compiled output, JAR dependencies)
   * @param targetDir the directory to copy the files into
   *
   * @return the number of files extracted
   * @throws IOException if a file cannot be read or written
   */
  public int extract(Set<URI> classpathRoots, Path targetDir) throws IOException {
    if (classpathRoots == null || classpathRoots.isEmpty()) {
      return 0;
    }

    int count = 0;
    ClassGraph graph =
        new ClassGraph().overrideClasspath(classpathRoots).acceptPaths(RESOURCE_ROOT);
    try (ScanResult scan = graph.scan()) {
      for (Resource resource : scan.getAllResources()) {
        String path = resource.getPath();
        if (!path.startsWith(RESOURCE_ROOT + "/")) {
          continue;
        }

        Path dest = targetDir.resolve(path.substring(RESOURCE_ROOT.length() + 1)).normalize();
        if (!dest.startsWith(targetDir)) {
          continue;
        }

        Files.createDirectories(dest.getParent());
        Files.write(dest, resource.load());
        count++;
      }
    }

    return count;
  }
}
