package com.webforj.plugin.maven.bbj;

import com.webforj.bbjship.foundation.Deployment;
import com.webforj.bbjship.foundation.DeploymentArchive;
import com.webforj.plugin.maven.resolve.MavenArtifacts;
import java.io.File;
import java.nio.file.Path;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * Creates the BBj deployment of a packaged Maven project.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ProjectDeployment {

  private ProjectDeployment() {}

  /**
   * Creates the deployment that writes the archive of a project to {@code bbj/<publishName>.zip} in
   * its build directory.
   *
   * @param project the packaged project
   * @param publishName the publish name the archive is named after
   * @param goal the goal the message names when the packaged jar is missing
   *
   * @return the deployment of the project
   * @throws MojoExecutionException if the publish name is blank or invalid, the project is not
   *         packaged as a jar or the packaged jar is missing
   */
  public static Deployment create(MavenProject project, String publishName, String goal)
      throws MojoExecutionException {
    Path archive = resolveArchive(project, publishName);
    if (!"jar".equals(project.getPackaging())) {
      throw new MojoExecutionException("BBj deployment requires jar packaging, this project is "
          + "packaged as " + project.getPackaging());
    }

    File application = project.getArtifact().getFile();
    if (application == null) {
      throw new MojoExecutionException("BBj deployment requires the packaged application jar. "
          + "Run the package phase in the same invocation, as in mvn package webforj:" + goal);
    }

    return Deployment.create(archive).setClasspath(MavenArtifacts.getRuntimeClasspath(project))
        .setApplication(application.toPath()).build();
  }

  /**
   * Resolves the deployment archive of a project, {@code bbj/<publishName>.zip} in its build
   * directory.
   *
   * @param project the project
   * @param publishName the publish name the archive is named after
   *
   * @return the archive
   * @throws MojoExecutionException if the publish name is blank or invalid
   */
  public static Path resolveArchive(MavenProject project, String publishName)
      throws MojoExecutionException {
    try {
      return DeploymentArchive.resolveArchive(Path.of(project.getBuild().getDirectory()),
          publishName);
    } catch (IllegalArgumentException refusal) {
      throw new MojoExecutionException(refusal.getMessage(), refusal);
    }
  }
}
