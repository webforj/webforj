package com.webforj.plugin.maven;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.List;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.model.PluginExecution;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

class WebforjLifecycleParticipantTest {

  @Test
  void shouldBindBundleTestAndCleanGoals() {
    MavenProject project = projectWithPlugin();

    runParticipant(project);

    Plugin plugin = webforjPlugin(project);
    assertExecution(plugin, "prepare-package", "bundle");
    assertExecution(plugin, "test", "test");
    assertExecution(plugin, "clean", "clean");
  }

  @Test
  void shouldBindEachGoalOnlyOnce() {
    MavenProject project = projectWithPlugin();

    runParticipant(project);
    runParticipant(project);

    assertEquals(3, webforjPlugin(project).getExecutions().size());
  }

  @Test
  void shouldKeepAnExistingGoalBinding() {
    MavenProject project = projectWithPlugin();
    final Plugin plugin = webforjPlugin(project);
    PluginExecution custom = new PluginExecution();
    custom.setId("my-bundle");
    custom.setPhase("package");
    custom.addGoal("bundle");
    plugin.addExecution(custom);

    runParticipant(project);

    long bundleBindings = plugin.getExecutions().stream()
        .filter(execution -> execution.getGoals().contains("bundle")).count();
    assertEquals(1, bundleBindings);
  }

  @Test
  void shouldCopyPluginConfigurationToBoundGoals() {
    MavenProject project = projectWithPlugin();
    Plugin plugin = webforjPlugin(project);
    Xpp3Dom configuration = new Xpp3Dom("configuration");
    Xpp3Dom eager = new Xpp3Dom("eager");
    eager.setValue("true");
    configuration.addChild(eager);
    plugin.setConfiguration(configuration);

    runParticipant(project);

    for (PluginExecution execution : plugin.getExecutions()) {
      Xpp3Dom copy = (Xpp3Dom) execution.getConfiguration();
      assertNotNull(copy, "expected configuration on execution " + execution.getId());
      assertNotSame(configuration, copy);
      assertEquals("true", copy.getChild("eager").getValue());
    }
  }

  @Test
  void shouldLeaveBoundGoalsWithoutConfigurationWhenThePluginHasNone() {
    MavenProject project = projectWithPlugin();

    runParticipant(project);

    for (PluginExecution execution : webforjPlugin(project).getExecutions()) {
      assertNull(execution.getConfiguration());
    }
  }

  @Test
  void shouldIgnoreProjectsWithoutThePlugin() {
    MavenProject project = new MavenProject();
    project.setBuild(new Build());

    runParticipant(project);

    assertTrue(project.getBuildPlugins().isEmpty());
  }

  private void runParticipant(MavenProject project) {
    MavenSession session = Mockito.mock(MavenSession.class);
    when(session.getProjects()).thenReturn(List.of(project));
    new WebforjLifecycleParticipant().afterProjectsRead(session);
  }

  private MavenProject projectWithPlugin() {
    final MavenProject project = new MavenProject();
    Build build = new Build();
    Plugin plugin = new Plugin();
    plugin.setGroupId("com.webforj");
    plugin.setArtifactId("webforj-maven-plugin");
    build.addPlugin(plugin);
    project.setBuild(build);

    return project;
  }

  private Plugin webforjPlugin(MavenProject project) {
    return project.getBuildPlugins().get(0);
  }

  private void assertExecution(Plugin plugin, String phase, String goal) {
    boolean found = plugin.getExecutions().stream().anyMatch(
        execution -> phase.equals(execution.getPhase()) && execution.getGoals().contains(goal));
    assertTrue(found, "expected execution " + goal + "@" + phase);
  }
}
