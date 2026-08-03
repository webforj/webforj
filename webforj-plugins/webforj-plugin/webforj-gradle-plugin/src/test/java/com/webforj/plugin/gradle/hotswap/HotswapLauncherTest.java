package com.webforj.plugin.gradle.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.gradle.WebforjExtension;
import com.webforj.plugin.gradle.WebforjPlugin;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HotswapLauncherTest {

  private Project project;
  private WebforjExtension extension;

  @BeforeEach
  void setUp() {
    project = ProjectBuilder.builder().build();
    project.getPlugins().apply("java");
    project.getPlugins().apply(WebforjPlugin.class);
    extension = project.getExtensions().getByType(WebforjExtension.class);
  }

  @Test
  void shouldStayOffWithoutAnyConfiguration() {
    List<String> arguments =
        HotswapLauncher.arguments(extension.getHotswap(), null, false, project.getLogger());

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldComposeTheJrebelArguments(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments =
        HotswapLauncher.arguments(extension.getHotswap(), null, false, project.getLogger());

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath()), arguments);
  }

  @Test
  void shouldComposeTheArgumentsWhenThePathIsSetWithoutTheBlock(@TempDir Path tmp)
      throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    // The path lands on the nested configuration directly, the form the property accessors of the
    // Kotlin build language produce, without the configuration block ever running.
    extension.getHotswap().getJrebel().getPath().set(project.file(library));

    List<String> arguments =
        HotswapLauncher.arguments(extension.getHotswap(), null, false, project.getLogger());

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath()), arguments);
  }

  @Test
  void shouldSwitchTheSpringDevelopmentRestartOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments =
        HotswapLauncher.arguments(extension.getHotswap(), null, true, project.getLogger());

    assertTrue(arguments.contains(HotswapLauncher.SPRING_RESTART_OFF),
        "the development restart cannot race the redefinition");
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments =
        HotswapLauncher.arguments(extension.getHotswap(), "off", false, project.getLogger());

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue() {
    assertThrows(GradleException.class, () -> HotswapLauncher.arguments(extension.getHotswap(),
        "dcevm", false, project.getLogger()));
  }

  @Test
  void shouldRequireTheJrebelPath() {
    extension.hotswap(hotswap -> hotswap.jrebel(jrebel -> {
    }));

    GradleException failure = assertThrows(GradleException.class,
        () -> HotswapLauncher.arguments(extension.getHotswap(), null, false, project.getLogger()));

    assertTrue(failure.getMessage().contains("jrebel path"));
  }
}
