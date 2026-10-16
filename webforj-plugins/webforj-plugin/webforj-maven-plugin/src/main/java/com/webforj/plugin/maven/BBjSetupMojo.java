package com.webforj.plugin.maven;

import com.webforj.bbjship.foundation.Setup;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.plugin.maven.bbj.DeploymentSettings;
import com.webforj.plugin.maven.bbj.model.BBjOptions;
import com.webforj.plugin.maven.resolve.MavenArtifacts;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import javax.inject.Inject;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.resolution.ArtifactResolutionException;

/**
 * Sets up the webforJ plugin in a local BBj installation.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
@Mojo(name = "bbj-setup", threadSafe = true)
public final class BBjSetupMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", readonly = true, required = true)
  MavenProject project;

  @Parameter
  BBjOptions bbj = new BBjOptions();

  @Parameter(property = "webforj.bbj.deployment.url")
  String deploymentUrl;

  @Parameter(property = "webforj.bbj.admin.username")
  String username;

  @Parameter(property = "webforj.bbj.admin.password")
  String password;

  @Parameter(property = "webforj.bbj.home")
  File home;

  @Parameter(property = "webforj.bbj.force")
  boolean force;

  @Parameter(defaultValue = "${repositorySystemSession}", readonly = true, required = true)
  RepositorySystemSession repositorySession;

  @Parameter(defaultValue = "${plugin}", readonly = true, required = true)
  PluginDescriptor pluginDescriptor;

  private final RepositorySystem repositorySystem;

  /**
   * Creates the goal with the repository system the BBjShip package is resolved through.
   *
   * @param repositorySystem the repository system of the build
   */
  @Inject
  public BBjSetupMojo(RepositorySystem repositorySystem) {
    this.repositorySystem = repositorySystem;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void execute() throws MojoExecutionException {
    DeploymentSettings configured = DeploymentSettings.create(bbj).setUrl(deploymentUrl)
        .setUsername(username).setPassword(password).setHome(home);
    BBjCredentials credentials = configured.getPasswordCredentials(getLog()::warn);
    try {
      URI deploymentUri = configured.resolveUrl();
      Path bundle = MavenArtifacts.resolveBBjShipPackage(repositorySystem, repositorySession,
          project.getRemoteProjectRepositories(), pluginDescriptor.getVersion());
      Setup.Builder setup = Setup.create(bundle, pluginDescriptor.getVersion())
          .setDeploymentUrl(deploymentUri).setCredentials(credentials).setPort(configured.getPort())
          .setForce(force).setLog(getLog()::info).setWarn(getLog()::warn);
      File effectiveHome = configured.getHome();
      if (effectiveHome != null) {
        setup.setHome(effectiveHome.toPath());
      }

      setup.build().apply();
    } catch (ArtifactResolutionException | IOException | IllegalArgumentException
        | IllegalStateException failure) {
      throw new MojoExecutionException(
          "BBj setup failed. Check the BBj plugin artifact and setup settings", failure);
    }
  }
}
