package com.webforj.plugin.maven;

import com.webforj.plugin.foundation.bbj.Deployment;
import com.webforj.plugin.maven.bbj.DeploymentSettings;
import com.webforj.plugin.maven.bbj.ProjectDeployment;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

/**
 * Writes the BBj deployment archive of a packaged application without deploying it.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Mojo(name = "bbj-package", defaultPhase = LifecyclePhase.PACKAGE,
    requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true)
public final class BBjPackageMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  MavenProject project;

  @Parameter
  BBjOptions bbj = new BBjOptions();

  @Parameter(property = "webforj.bbj.publishName")
  String publishName;

  /**
   * {@inheritDoc}
   */
  @Override
  public void execute() throws MojoExecutionException {
    DeploymentSettings settings = DeploymentSettings.create(bbj).setPublishName(publishName)
        .setDefaultPublishName(project.getBuild().getFinalName());
    Deployment deployment =
        ProjectDeployment.create(project, settings.getPublishName(), "bbj-package");
    try {
      Path archive = deployment.writeArchive();
      getLog().info("BBj deployment archive written to " + archive);
    } catch (IOException failure) {
      throw new MojoExecutionException("BBj deployment archive could not be written. Check the "
          + "application package and dependencies", failure);
    }
  }
}
