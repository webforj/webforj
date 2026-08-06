package com.webforj.plugin.foundation.hotswap.hotswapagent;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import java.io.IOException;
import java.nio.file.Path;

/**
 * Resolves the published redefinition observer jar for the application virtual machine.
 *
 * <p>
 * The observer is resolved at the framework version found on the application classpath, the version
 * of the live reload receiver it reports to.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class HotswapObserverJar {

  /** The group of the published observer artifact. */
  public static final String GROUP_ID = "com.webforj";

  /** The artifact name of the published observer artifact. */
  public static final String ARTIFACT_ID = "webforj-hotswap-observer";

  private HotswapObserverJar() {}

  /**
   * Resolves the observer jar at the framework version of the application.
   *
   * @param applicationClasspath the resolved runtime classpath of the application
   * @param resolver the resolver the observer artifact is resolved through
   *
   * @return the observer jar location on disk
   * @throws IOException if the application carries no webforJ framework on its classpath or the
   *         observer cannot be resolved
   */
  public static Path resolve(ApplicationClasspath applicationClasspath, ArtifactResolver resolver)
      throws IOException {
    String version = applicationClasspath.getFrameworkVersion()
        .orElseThrow(() -> new IOException(
            "hotswap needs the webforJ framework on the application classpath, no "
                + ApplicationClasspath.FRAMEWORK_GROUP_ID + ":"
                + ApplicationClasspath.FRAMEWORK_ARTIFACT_ID + " dependency was found"));

    for (ResolvedJar jar : resolver.resolve(GROUP_ID, ARTIFACT_ID, version)) {
      if (GROUP_ID.equals(jar.groupId()) && ARTIFACT_ID.equals(jar.artifactId())) {
        return jar.file();
      }
    }

    throw new IOException("the resolution of " + GROUP_ID + ":" + ARTIFACT_ID + ":" + version
        + " returned no observer jar");
  }
}
