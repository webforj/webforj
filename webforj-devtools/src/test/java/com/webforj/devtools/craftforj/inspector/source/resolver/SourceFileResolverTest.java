package com.webforj.devtools.craftforj.inspector.source.resolver;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.typesafe.config.ConfigFactory;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.ProjectRootResolver;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

class SourceFileResolverTest {

  @TempDir
  Path tempDir;

  @Test
  @DisplayName("Should resolve a project class to its source file")
  void shouldResolveProjectClassSource() {
    String path = SourceFileResolver.resolve(StylesheetResolver.class.getName(),
        SourceFileResolver.JAVA_ONLY);

    assertNotNull(path);
    assertTrue(path.endsWith("StylesheetResolver.java"));
  }

  @Test
  @DisplayName("Should return null for a class loaded from a jar")
  void shouldReturnNullForJarClass() {
    assertNull(SourceFileResolver.resolve("com.google.gson.Gson", SourceFileResolver.JAVA_ONLY));
  }

  @Test
  @DisplayName("Should return null for an unknown class")
  void shouldReturnNullForUnknownClass() {
    assertNull(SourceFileResolver.resolve("no.such.Clazz", SourceFileResolver.ALL_EXTENSIONS));
  }

  @Test
  @DisplayName("Should resolve a jar class under the configured project root")
  void shouldResolveJarClassUnderConfiguredRoot() throws Exception {
    Path sourceFile = tempDir.resolve(Path.of("src", "main", "java", "com", "typesafe", "config"))
        .resolve("ConfigFactory.java");
    Files.createDirectories(sourceFile.getParent());
    Files.createFile(sourceFile);

    Environment env = mock(Environment.class);
    when(env.getConfig()).thenReturn(
        ConfigFactory.parseString(ProjectRootResolver.KEY_PROJECT_ROOT + " = \"" + tempDir + "\""));

    try (MockedStatic<Environment> envMock = mockStatic(Environment.class)) {
      envMock.when(Environment::getCurrent).thenReturn(env);

      String path = SourceFileResolver.resolve("com.typesafe.config.ConfigFactory",
          SourceFileResolver.JAVA_ONLY);

      assertEquals(sourceFile.toAbsolutePath().toString(), path);
    }
  }
}
