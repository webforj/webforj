package com.webforj.plugin.maven;

import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.PublishNamePolicy;
import com.webforj.plugin.maven.bbj.DeploymentSettings;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import java.io.IOException;
import java.util.Optional;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

/**
 * Removes a published application from BBjServices.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Mojo(name = "bbj-undeploy", threadSafe = true)
public final class BBjUndeployMojo extends AbstractMojo {

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
    Optional<String> refusal = PublishNamePolicy.findRefusal(settings.getPublishName());
    if (refusal.isPresent()) {
      throw new MojoExecutionException(refusal.get());
    }

    try {
      BBjShipClient bbjShip = settings.createBBjShipClient(getLog());
      getLog().info("Undeploying " + settings.getPublishName() + " from " + settings.getUrl());
      bbjShip.undeploy(settings.getPublishName());
      getLog().info("BBj undeployment of " + settings.getPublishName() + " completed");
    } catch (IOException | IllegalArgumentException failure) {
      throw new MojoExecutionException("BBj undeployment failed. "
          + "Check the endpoint, administration credentials and BBjServices log", failure);
    }
  }
}
