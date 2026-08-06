package com.webforj.plugin.foundation.resolve;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.IOException;
import java.util.List;

/**
 * Resolves a module and its runtime dependencies through the repositories of the build.
 *
 * <p>
 * Each build system contributes one implementation over its own resolution machinery. Everything
 * consuming the result works on the neutral {@link ResolvedJar} form, so the consuming logic is
 * written once.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
@FunctionalInterface
public interface ArtifactResolver {

  /**
   * Resolves the named module and its runtime dependencies.
   *
   * @param groupId the group of the module
   * @param artifactId the artifact name of the module
   * @param version the version to resolve at
   *
   * @return the resolved jars
   * @throws IOException if the resolution fails
   */
  List<ResolvedJar> resolve(String groupId, String artifactId, String version) throws IOException;
}
