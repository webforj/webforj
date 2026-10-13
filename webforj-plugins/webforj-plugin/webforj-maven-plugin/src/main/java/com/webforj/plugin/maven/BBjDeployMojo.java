package com.webforj.plugin.maven;

import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.Deployment;
import com.webforj.plugin.maven.bbj.DeploymentSettings;
import com.webforj.plugin.maven.bbj.ProjectDeployment;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

/**
 * Deploys a packaged application to BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Mojo(name = "bbj-deploy", defaultPhase = LifecyclePhase.INSTALL,
    requiresDependencyResolution = ResolutionScope.RUNTIME, threadSafe = true)
public final class BBjDeployMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  MavenProject project;

  @Parameter(defaultValue = "${plugin}", readonly = true, required = true)
  PluginDescriptor pluginDescriptor;

  @Parameter
  BBjOptions bbj = new BBjOptions();

  @Parameter(property = "webforj.bbj.deployment.url")
  String deploymentUrl;

  @Parameter(property = "webforj.bbj.admin.username")
  String username;

  @Parameter(property = "webforj.bbj.admin.password")
  String password;

  @Parameter(property = "webforj.bbj.admin.token")
  String token;

  @Parameter(property = "webforj.bbj.publishName")
  String publishName;

  /**
   * {@inheritDoc}
   */
  @Override
  public void execute() throws MojoExecutionException {
    DeploymentSettings settings =
        DeploymentSettings.create(bbj).setUrl(deploymentUrl).setPublishName(publishName)
            .setDefaultPublishName(project.getBuild().getFinalName()).setUsername(username)
            .setPassword(password).setToken(token).setVersion(pluginDescriptor.getVersion());
    Deployment deployment =
        ProjectDeployment.create(project, settings.getPublishName(), "bbj-deploy");
    try {
      BBjShipClient bbjShip = settings.createBBjShipClient(getLog());
      getLog().info("Deploying " + settings.getPublishName() + " to " + settings.getUrl());
      Path archive = deployment.writeArchive();
      URI published = bbjShip.deploy(archive, settings.getPublishName());
      getLog()
          .info("BBj deployment of " + settings.getPublishName() + " completed at " + published);
    } catch (IOException | IllegalArgumentException failure) {
      throw new MojoExecutionException("BBj deployment failed. "
          + "Check the application package, dependencies and deployment settings", failure);
    }
  }
}
