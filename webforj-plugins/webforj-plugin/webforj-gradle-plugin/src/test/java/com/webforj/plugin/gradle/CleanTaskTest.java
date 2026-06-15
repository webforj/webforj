package com.webforj.plugin.gradle;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CleanTaskTest {

  private static CleanTask task(Project project, Path sourceRoot) {
    CleanTask task = project.getTasks().create("webforjCleanFrontendTest", CleanTask.class);
    task.getSourceRoot().set(sourceRoot.toFile());

    return task;
  }

  @Test
  void shouldRemoveTheGeneratedDirectory(@TempDir Path tmp) throws Exception {
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    Path sourceRoot = tmp.resolve("src/main/frontend");
    Path generated = sourceRoot.resolve("generated");
    Files.createDirectories(generated.resolve("react"));
    Files.writeString(generated.resolve("react/card.tsx"), "x");

    task(project, sourceRoot).clean();

    assertFalse(Files.exists(generated), "the generated frontend is removed");
  }

  @Test
  void shouldDoNothingWhenThereIsNoGeneratedDirectory(@TempDir Path tmp) {
    Project project = ProjectBuilder.builder().withProjectDir(tmp.toFile()).build();
    Path sourceRoot = tmp.resolve("src/main/frontend");

    task(project, sourceRoot).clean();

    assertTrue(Files.notExists(sourceRoot.resolve("generated")), "nothing is created");
  }
}
