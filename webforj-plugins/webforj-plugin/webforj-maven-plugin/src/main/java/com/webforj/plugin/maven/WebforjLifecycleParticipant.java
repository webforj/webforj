package com.webforj.plugin.maven;

import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;

/**
 * Binds the bundle, frontend test, and clean goals to the build automatically, so an application
 * enables the whole bundler integration with a single plugin declaration and no execution blocks.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class WebforjLifecycleParticipant extends AbstractMavenLifecycleParticipant {

  private static final String GROUP_ID = "com.webforj";
  private static final String ARTIFACT_ID = "webforj-maven-plugin";

  /**
   * {@inheritDoc}
   */
  @Override
  public void afterProjectsRead(MavenSession session) {
    for (MavenProject project : session.getProjects()) {
      Plugin plugin = findPlugin(project);
      if (plugin != null) {
        ensureExecution(plugin, "default-webforj-bundle", "prepare-package", "bundle");
        ensureExecution(plugin, "default-webforj-test", "test", "test");
        ensureExecution(plugin, "default-webforj-clean", "clean", "clean");
      }
    }
  }

  private Plugin findPlugin(MavenProject project) {
    for (Plugin plugin : project.getBuildPlugins()) {
      if (GROUP_ID.equals(plugin.getGroupId()) && ARTIFACT_ID.equals(plugin.getArtifactId())) {
        return plugin;
      }
    }

    return null;
  }

  private void ensureExecution(Plugin plugin, String id, String phase, String goal) {
    for (PluginExecution existing : plugin.getExecutions()) {
      if (existing.getGoals().contains(goal)) {
        return;
      }
    }

    PluginExecution execution = new PluginExecution();
    execution.setId(id);
    execution.setPhase(phase);
    execution.addGoal(goal);

    // Maven pushes the plugin level configuration into executions during model building, which
    // has already run by the time this participant adds an execution, so the configuration is
    // copied here or it would never reach the goal.
    Object configuration = plugin.getConfiguration();
    if (configuration instanceof Xpp3Dom dom) {
      execution.setConfiguration(new Xpp3Dom(dom));
    }

    plugin.addExecution(execution);
    plugin.flushExecutionMap();
  }
}
