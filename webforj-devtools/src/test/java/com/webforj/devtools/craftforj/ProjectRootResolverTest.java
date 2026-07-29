package com.webforj.devtools.craftforj;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ProjectRootResolverTest {

  @TempDir
  Path tempDir;

  @Test
  @DisplayName("Should prefer the configured project root")
  void shouldPreferConfiguredRoot() {
    Config config = configWithRoot(tempDir.toString());

    Path root = ProjectRootResolver.resolve(config, ProjectRootResolver.class);

    assertEquals(tempDir.toAbsolutePath().normalize(), root);
  }

  @Test
  @DisplayName("Should ignore a configured root that is not a directory")
  void shouldIgnoreConfiguredRootThatIsNotDirectory() {
    Config config = configWithRoot(tempDir.resolve("missing").toString());

    Path root = ProjectRootResolver.resolve(config, ProjectRootResolver.class);

    assertTrue(Files.exists(root.resolve("pom.xml")));
  }

  @Test
  @DisplayName("Should ignore a blank configured root")
  void shouldIgnoreBlankConfiguredRoot() {
    assertNull(ProjectRootResolver.readConfiguredRoot(configWithRoot("  ")));
  }

  @Test
  @DisplayName("Should read no root from a null config")
  void shouldReadNoRootFromNullConfig() {
    assertNull(ProjectRootResolver.readConfiguredRoot(null));
  }

  @Test
  @DisplayName("Should derive the root from exploded classes by walking up to a build file")
  void shouldDeriveRootFromExplodedClasses() {
    Path root = ProjectRootResolver.resolve(null, ProjectRootResolver.class);

    assertTrue(Files.exists(root.resolve("pom.xml")));
    assertTrue(root.endsWith("webforj-devtools"));
  }

  @Test
  @DisplayName("Should fall back to the working directory when nothing can be derived")
  void shouldFallBackToWorkingDirectory() {
    Path root = ProjectRootResolver.resolve(null, String.class);

    assertEquals(Path.of(System.getProperty("user.dir")), root);
  }

  @Test
  @DisplayName("Should derive no root when no build file exists above the location")
  void shouldDeriveNoRootWithoutBuildFile() {
    assertNull(ProjectRootResolver.deriveFromCodeSource(String.class));
  }

  private Config configWithRoot(String value) {
    return ConfigFactory.parseString(ProjectRootResolver.KEY_PROJECT_ROOT + " = \"" + value + "\"");
  }
}
