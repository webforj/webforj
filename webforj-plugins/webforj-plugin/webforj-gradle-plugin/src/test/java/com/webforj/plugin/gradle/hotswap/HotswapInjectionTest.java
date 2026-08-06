package com.webforj.plugin.gradle.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.foundation.resolve.ArtifactResolver;
import com.webforj.plugin.gradle.WebforjExtension;
import com.webforj.plugin.gradle.WebforjPlugin;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class HotswapInjectionTest {

  private static final String VERSION = "26.02-SNAPSHOT";

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
    List<String> arguments = injection().build().getArguments();

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldComposeTheJrebelArguments(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = injection().build().getArguments();

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath(), "-Dwebforj.hotswap.tool=jrebel",
        "-Dwebforj.hotswap.level=full"), arguments);
  }

  @Test
  void shouldComposeTheArgumentsWhenThePathIsSetWithoutTheBlock(@TempDir Path tmp)
      throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    // The path lands on the nested configuration directly, the form the property accessors of the
    // Kotlin build language produce, without the configuration block ever running.
    extension.getHotswap().getJrebel().getPath().set(project.file(library));

    List<String> arguments = injection().build().getArguments();

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath(), "-Dwebforj.hotswap.tool=jrebel",
        "-Dwebforj.hotswap.level=full"), arguments);
  }

  @Test
  void shouldComposeTheHotswapAgentArguments(@TempDir Path tmp) throws Exception {
    Path jar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    Path observer = Files.createFile(tmp.resolve("webforj-hotswap-observer.jar"));
    Path java = fakeJava(tmp, 0);
    List<String> versions = new ArrayList<>();
    extension
        .hotswap(hotswap -> hotswap.hotswapAgent(agent -> agent.getPath().set(project.file(jar))));

    List<String> arguments =
        injection().setJavaExecutable(java).setAgentCacheRoot(tmp.resolve("cache"))
            .setApplicationClasspath(HotswapInjectionTest::classpathWithFramework)
            .setResolver((groupId, artifactId, version) -> {
              versions.add(version);
              return List.of(new ResolvedJar(groupId, artifactId, version, observer));
            }).build().getArguments();

    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"));
    assertTrue(arguments.stream()
        .anyMatch(argument -> argument.startsWith("-javaagent:" + jar.toAbsolutePath())
            && argument.contains("autoHotswap=true")));
    assertTrue(arguments.contains("-javaagent:" + observer.toAbsolutePath()),
        "the resolved observer attaches behind the agent");
    assertEquals(List.of(VERSION), versions,
        "the observer resolves at the framework version of the application");
  }

  @Test
  void shouldSwitchTheSpringDevelopmentRestartOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = injection().setSpringBootRunner(true).build().getArguments();

    assertTrue(arguments.contains("-Dspring.devtools.restart.enabled=false"),
        "the development restart cannot race the redefinition");
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension
        .hotswap(hotswap -> hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library))));

    List<String> arguments = injection().setCommandLineValue("off").build().getArguments();

    assertTrue(arguments.isEmpty());
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue() {
    HotswapInjection unknown = injection().setCommandLineValue("dcevm").build();

    assertThrows(GradleException.class, unknown::getArguments);
  }

  @Test
  void shouldRequireTheJrebelPath() {
    extension.hotswap(hotswap -> hotswap.jrebel(jrebel -> {
    }));
    HotswapInjection incomplete = injection().build();

    GradleException failure = assertThrows(GradleException.class, incomplete::getArguments);

    assertTrue(failure.getMessage().contains("jrebel path"));
  }

  @Test
  void shouldFailWhenBothToolsAreConfigured(@TempDir Path tmp) throws Exception {
    Path library = Files.createFile(tmp.resolve("libjrebel64.dylib"));
    extension.hotswap(hotswap -> {
      hotswap.jrebel(jrebel -> jrebel.getPath().set(project.file(library)));
      hotswap.hotswapAgent(agent -> {
      });
    });
    HotswapInjection ambiguous = injection().build();

    GradleException failure = assertThrows(GradleException.class, ambiguous::getArguments);

    assertTrue(failure.getMessage().contains("hotswapAgent"));
    assertTrue(failure.getMessage().contains("jrebel"));
  }

  private HotswapInjection.Builder injection() {
    return HotswapInjection.create().setProject(project).setOptions(extension.getHotswap())
        .setBuildDirectory(buildDirectory()).setLog(project.getLogger())
        .setApplicationClasspath(neverAsked()).setResolver(neverResolved());
  }

  private Path buildDirectory() {
    return project.getLayout().getBuildDirectory().get().getAsFile().toPath();
  }

  private static ApplicationClasspath classpathWithFramework() {
    return new ApplicationClasspath(List.of(new ResolvedJar(ApplicationClasspath.FRAMEWORK_GROUP_ID,
        ApplicationClasspath.FRAMEWORK_ARTIFACT_ID, VERSION, Path.of("webforj-foundation.jar"))));
  }

  private static Supplier<ApplicationClasspath> neverAsked() {
    return () -> {
      throw new AssertionError("the classpath is asked only when the hotswap agent attaches");
    };
  }

  private static ArtifactResolver neverResolved() {
    return (groupId, artifactId, version) -> {
      throw new AssertionError("the observer resolves only when the hotswap agent attaches");
    };
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
