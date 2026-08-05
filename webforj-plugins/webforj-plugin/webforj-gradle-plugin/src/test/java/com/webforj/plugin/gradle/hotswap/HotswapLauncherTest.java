package com.webforj.plugin.gradle.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.gradle.WebforjExtension;
import com.webforj.plugin.gradle.WebforjPlugin;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Assumptions;
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
    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, false,
        buildDirectory(), null, project.getLogger());

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldComposeTheJrebelArguments(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, false,
        buildDirectory(), null, project.getLogger());

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath()), arguments);
  }

  @Test
  void shouldComposeTheArgumentsWhenThePathIsSetWithoutTheBlock(@TempDir Path tmp)
      throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    // The path lands on the nested configuration directly, the form the property accessors of the
    // Kotlin build language produce, without the configuration block ever running.
    extension.getHotswap().getJrebel().getPath().set(project.file(library));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, false,
        buildDirectory(), null, project.getLogger());

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath()), arguments);
  }

  @Test
  void shouldComposeTheHotswapAgentArguments(@TempDir Path tmp) throws Exception {
    Path jar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    Path java = fakeJava(tmp, 0);
    extension
        .hotswap(hotswap -> hotswap.hotswapAgent(agent -> agent.getPath().set(project.file(jar))));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, false,
        buildDirectory(), project.getLogger(), tmp.resolve("cache"), java);

    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"));
    assertTrue(arguments.stream()
        .anyMatch(argument -> argument.startsWith("-javaagent:" + jar.toAbsolutePath())
            && argument.contains("autoHotswap=true") && argument.contains("propertiesFilePath=")));

    Path properties = buildDirectory().resolve("hotswap").resolve("hotswap-agent.properties");
    assertTrue(Files.isRegularFile(properties));
    assertTrue(
        Files.readString(properties).contains("pluginPackages=com.webforj.devtools.hotswap"));
  }

  @Test
  void shouldAttachWithoutTheFlagOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    Path jar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    Path java = fakeJava(tmp, 1);
    extension
        .hotswap(hotswap -> hotswap.hotswapAgent(agent -> agent.getPath().set(project.file(jar))));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, false,
        buildDirectory(), project.getLogger(), tmp.resolve("cache"), java);

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("-javaagent:")),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals),
        "the unsupported flag never reaches the virtual machine");
  }

  @Test
  void shouldFailWhenBothToolsAreConfigured(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension.hotswap(hotswap -> {
      hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library)));
      hotswap.hotswapAgent(agent -> {
      });
    });

    GradleException failure =
        assertThrows(GradleException.class, () -> HotswapLauncher.arguments(extension.getHotswap(),
            null, false, buildDirectory(), null, project.getLogger()));

    assertTrue(failure.getMessage().contains("hotswapAgent"));
    assertTrue(failure.getMessage().contains("jrebel"));
  }

  @Test
  void shouldSwitchTheSpringDevelopmentRestartOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), null, true,
        buildDirectory(), null, project.getLogger());

    assertTrue(arguments.contains(HotswapLauncher.SPRING_RESTART_OFF),
        "the development restart cannot race the redefinition");
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = HotswapLauncher.arguments(extension.getHotswap(), "off", false,
        buildDirectory(), null, project.getLogger());

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue() {
    assertThrows(GradleException.class, () -> HotswapLauncher.arguments(extension.getHotswap(),
        "dcevm", false, buildDirectory(), null, project.getLogger()));
  }

  @Test
  void shouldRequireTheJrebelPath() {
    extension.hotswap(hotswap -> hotswap.jrebel(jrebel -> {
    }));

    GradleException failure =
        assertThrows(GradleException.class, () -> HotswapLauncher.arguments(extension.getHotswap(),
            null, false, buildDirectory(), null, project.getLogger()));

    assertTrue(failure.getMessage().contains("jrebel path"));
  }

  private Path buildDirectory() {
    return project.getLayout().getBuildDirectory().get().getAsFile().toPath();
  }

  private static Path fakeJava(Path dir, int exitCode) throws IOException {
    Assumptions.assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"),
        "the capability check stand in needs a posix file system");

    Path script = dir.resolve("java");
    Files.writeString(script, "#!/bin/sh\nexit " + exitCode + "\n");
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"));

    return script;
  }
}
