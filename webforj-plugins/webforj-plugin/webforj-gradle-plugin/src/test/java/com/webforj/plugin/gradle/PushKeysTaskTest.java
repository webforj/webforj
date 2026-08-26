package com.webforj.plugin.gradle;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;

class PushKeysTaskTest {

  @Test
  void shouldGenerateWithoutTheProjectConfiguration() {
    Project project = ProjectBuilder.builder().build();
    PushKeysTask task = project.getTasks().create("webPushKeysTest", PushKeysTask.class);

    assertDoesNotThrow(task::generate);
  }

  @Test
  void shouldBeRegisteredByThePluginUnderTheWebforjGroup() {
    Project project = ProjectBuilder.builder().build();
    project.getPlugins().apply("java");
    project.getPlugins().apply(WebforjPlugin.class);

    assertNotNull(project.getTasks().findByName("webforjPushKeys"));
    assertEquals("webforj", project.getTasks().getByName("webforjPushKeys").getGroup());
  }
}
