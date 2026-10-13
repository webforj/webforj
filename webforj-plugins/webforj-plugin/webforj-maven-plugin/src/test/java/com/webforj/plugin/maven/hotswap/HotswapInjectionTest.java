package com.webforj.plugin.maven.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.plugin.foundation.hotswap.HotswapLaunch;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import com.webforj.plugin.maven.hotswap.hotswapagent.HotswapAgentOptions;
import com.webforj.plugin.maven.hotswap.jrebel.JrebelOptions;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.model.Build;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

class HotswapInjectionTest {

  private static final String SPRING = "spring-boot-maven-plugin";
  private static final String JETTY = "jetty-ee11-maven-plugin";
  private static final String FRAMEWORK = "webforj-foundation";
  private static final String VERSION = "26.02";
  private static final String AGENT_PATH_OPTION = "-agentpath:";
  private static final String JAVA_AGENT_OPTION = "-javaagent:";
  private static final String HOTSWAP_DIRECTORY = "hotswap";
  private static final String OPTIONS_FILE = "hotswap/hotswap.options";
  private static final String DEPLOYMENT_URL = "http://localhost:8888/webforj-bbjship";

  @Test
  void shouldHandTheAgentToTheSpringBootFork(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.contains(AGENT_PATH_OPTION), "the agent flag reaches the fork arguments");
    assertTrue(arguments.contains("-Dspring.devtools.restart.enabled=false"),
        "the development restart cannot race the redefinition");
  }

  @Test
  void shouldAppendToTheProjectForkArguments(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    project.getProperties().setProperty(HotswapInjection.SPRING_JVM_ARGUMENTS, "-Xmx1g");

    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.startsWith("-Xmx1g "), "the build supplied arguments stay first");
    assertTrue(arguments.contains(AGENT_PATH_OPTION));
  }

  @Test
  void shouldMergeIntoTheCommandLineValueSoItStaysVisible(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    Properties userProperties = new Properties();
    userProperties.setProperty(HotswapInjection.SPRING_JVM_ARGUMENTS, "-Xmx1g");

    newInjection(project, userProperties, jrebelOptions(tmp), null).apply();

    String merged = userProperties.getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(merged.startsWith("-Xmx1g "), "the command line arguments stay first");
    assertTrue(merged.contains(AGENT_PATH_OPTION),
        "the agent lands in the command line properties that outrank the project properties");
  }

  @Test
  void shouldForkTheJettyRunnerAndHandTheAgentToIt(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, JETTY);
    newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    assertEquals("FORK", project.getProperties().getProperty(HotswapInjection.JETTY_DEPLOY_MODE));
    assertTrue(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS)
        .contains(AGENT_PATH_OPTION));
  }

  @Test
  void shouldNeverAttachToAnUnforkedJettyRunner(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, JETTY);
    Properties userProperties = new Properties();
    userProperties.setProperty(HotswapInjection.JETTY_DEPLOY_MODE, "EMBED");

    newInjection(project, userProperties, jrebelOptions(tmp), null).apply();

    // An unforked runner shares the build process, and the agent must never enter that one.
    assertNull(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS));
    assertEquals("EMBED", userProperties.getProperty(HotswapInjection.JETTY_DEPLOY_MODE));
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), jrebelOptions(tmp), "off").apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
  }

  @Test
  void shouldStayOffWithoutAnyConfiguration(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    newInjection(project, new Properties(), null, null).apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue(@TempDir Path tmp) {
    MavenProject project = newProject(tmp, SPRING);

    assertThrows(MojoExecutionException.class,
        () -> newInjection(project, new Properties(), null, "dcevm").apply());
  }

  @Test
  void shouldRequireTheJrebelPath(@TempDir Path tmp) {
    HotswapOptions options = new HotswapOptions().setJrebel(new JrebelOptions());
    MavenProject project = newProject(tmp, SPRING);

    MojoExecutionException failure = assertThrows(MojoExecutionException.class,
        () -> newInjection(project, new Properties(), options, null).apply());

    assertTrue(failure.getMessage().contains("jrebel path"));
  }

  @Test
  void shouldFailWhenTheBuildNamesBothTools(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    HotswapOptions options = jrebelOptions(tmp).setHotswapAgent(new HotswapAgentOptions());

    MojoExecutionException failure = assertThrows(MojoExecutionException.class,
        () -> newInjection(project, new Properties(), options, null).apply());

    assertTrue(failure.getMessage().contains("hotswapAgent and jrebel"));
  }

  @Test
  void shouldWarnWhenTheBuildHasNoSupportedRunner(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp);
    Log log = mock(Log.class);
    HotswapInjection.create().setProject(project).setUserProperties(new Properties())
        .setOptions(jrebelOptions(tmp)).setLog(log).build().apply();

    assertNull(project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
    assertNull(project.getProperties().getProperty(HotswapInjection.JETTY_JVM_ARGS));
    verify(log).warn(contains("no supported application runner"));
  }

  @Test
  void shouldHandTheHotswapAgentToTheSpringBootFork(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    givenFramework(project);
    List<String> versions = new ArrayList<>();
    Path observer = observerJar(tmp);

    HotswapInjection.create().setProject(project).setUserProperties(new Properties())
        .setOptions(hotswapAgentOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
        .setResolver((groupId, artifactId, version) -> {
          versions.add(version);
          return List.of(new ResolvedJar(groupId, artifactId, version, observer));
        }).setJavaExecutable(fakeJava(tmp, 0)).setLog(mock(Log.class)).build().apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.contains(JAVA_AGENT_OPTION), "the agent flag reaches the fork arguments");
    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"));
    assertTrue(arguments.contains(JAVA_AGENT_OPTION + observer.toAbsolutePath()),
        "the resolved observer jar reaches the fork arguments");
    assertEquals(List.of(VERSION), versions,
        "the observer resolves at the framework version of the application");
  }

  @Test
  void shouldWarnAndAttachLimitedOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    givenFramework(project);
    Log log = mock(Log.class);
    Path observer = observerJar(tmp);

    HotswapInjection.create().setProject(project).setUserProperties(new Properties())
        .setOptions(hotswapAgentOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
        .setResolver((groupId, artifactId, version) -> List
            .of(new ResolvedJar(groupId, artifactId, version, observer)))
        .setJavaExecutable(fakeJava(tmp, 1)).setLog(log).build().apply();

    String arguments = project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS);
    assertTrue(arguments.contains(JAVA_AGENT_OPTION),
        "the agent still attaches for the method body changes");
    assertFalse(arguments.contains("-XX:+AllowEnhancedClassRedefinition"),
        "the unsupported flag never reaches the virtual machine");
    verify(log).warn(contains("method body changes"));
  }

  @Test
  void shouldFailWhenTheApplicationHasNoFramework(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);
    when(project.getArtifacts()).thenReturn(Set.of());
    HotswapInjection injection =
        HotswapInjection.create().setProject(project).setUserProperties(new Properties())
            .setOptions(hotswapAgentOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
            .setResolver((groupId, artifactId, version) -> List.of())
            .setJavaExecutable(fakeJava(tmp, 0)).setLog(mock(Log.class)).build();

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, injection::apply);

    assertTrue(failure.getMessage().contains(FRAMEWORK), "the missing requirement is named");
  }

  @Test
  void shouldLogTheContainerArgumentAndInjectNothingForTheSharedContainer(@TempDir Path tmp)
      throws Exception {
    MavenProject project = newProject(tmp);
    Log log = mock(Log.class);

    HotswapInjection.create().setProject(project).setUserProperties(new Properties())
        .setOptions(jrebelOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
        .setBbjDeploymentUrl(DEPLOYMENT_URL).setLog(log).build().apply();

    verify(log)
        .info("webforJ hotswap: -XX:VMOptionsFile=" + tmp.resolve(OPTIONS_FILE).toAbsolutePath());
    verify(log, never()).warn(contains("no supported application runner"));
    assertEquals(new Properties(), project.getProperties());
  }

  @ParameterizedTest
  @ValueSource(strings = {SPRING, JETTY})
  void shouldHandTheAgentToTheRunnerWhenTheBbjProjectHasOne(String runner, @TempDir Path tmp)
      throws Exception {
    MavenProject project = newProject(tmp, runner);

    HotswapInjection.create().setProject(project).setUserProperties(new Properties())
        .setOptions(jrebelOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
        .setBbjDeploymentUrl(DEPLOYMENT_URL).setLog(mock(Log.class)).build().apply();

    String key = SPRING.equals(runner) ? HotswapInjection.SPRING_JVM_ARGUMENTS
        : HotswapInjection.JETTY_JVM_ARGS;
    assertTrue(project.getProperties().getProperty(key).contains(AGENT_PATH_OPTION));
    assertFalse(Files.exists(tmp.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldReturnNoArgumentsWhileHotswapStaysOff(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);

    List<String> arguments = newInjection(project, new Properties(), null, null).apply();

    assertEquals(List.of(), arguments);
  }

  @Test
  void shouldReturnTheContainerArgumentForTheSharedContainer(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp);

    List<String> arguments =
        HotswapInjection.create().setProject(project).setUserProperties(new Properties())
            .setOptions(jrebelOptions(tmp)).setDirectory(tmp.resolve(HOTSWAP_DIRECTORY))
            .setBbjDeploymentUrl(DEPLOYMENT_URL).setLog(mock(Log.class)).build().apply();

    assertEquals(List.of("-XX:VMOptionsFile=" + tmp.resolve(OPTIONS_FILE).toAbsolutePath()),
        arguments);
  }

  @Test
  void shouldReturnTheArgumentsHandedToTheRunner(@TempDir Path tmp) throws Exception {
    MavenProject project = newProject(tmp, SPRING);

    List<String> arguments =
        newInjection(project, new Properties(), jrebelOptions(tmp), null).apply();

    assertEquals(String.join(" ", arguments) + " -Dspring.devtools.restart.enabled=false",
        project.getProperties().getProperty(HotswapInjection.SPRING_JVM_ARGUMENTS));
  }

  @Test
  void shouldGiveTheHotswapDirectoryAsTheOptionsDirectory(@TempDir Path tmp) {
    HotswapInjection injection = HotswapInjection.create().setProject(newProject(tmp))
        .setDirectory(tmp.resolve(HOTSWAP_DIRECTORY)).setLog(mock(Log.class)).build();

    assertEquals(tmp.resolve(HOTSWAP_DIRECTORY), injection.getContainerOptionsDirectory());
  }

  @ParameterizedTest
  @CsvSource(value = {"configured,,configured", "configured,property,property"})
  void shouldPreferTheCommandLineDirectoryOverTheConfiguredOne(String configured,
      String commandLine, String expected, @TempDir Path tmp) {
    HotswapOptions options = new HotswapOptions().setDirectory(tmp.resolve(configured).toFile());
    Path commandLineDirectory = commandLine == null ? null : tmp.resolve(commandLine);

    HotswapInjection injection = HotswapInjection.create().setProject(newProject(tmp))
        .setOptions(options).setDirectory(commandLineDirectory).setLog(mock(Log.class)).build();

    assertEquals(tmp.resolve(expected), injection.getContainerOptionsDirectory());
  }

  @Test
  void shouldUseTheDefaultDirectoryWithoutAnyDirectory(@TempDir Path tmp) {
    HotswapInjection injection =
        HotswapInjection.create().setProject(newProject(tmp)).setLog(mock(Log.class)).build();

    assertEquals(HotswapLaunch.getDefaultDirectory(), injection.getContainerOptionsDirectory());
  }

  @Test
  void shouldEndTheGoalWhenTheHotswapDirectoryHasSpace(@TempDir Path tmp) throws Exception {
    Path directory = tmp.resolve("hotswap directory");
    HotswapInjection injection = HotswapInjection.create().setProject(newProject(tmp, SPRING))
        .setUserProperties(new Properties()).setOptions(hotswapAgentOptions(tmp))
        .setDirectory(directory).setLog(mock(Log.class)).build();

    MojoExecutionException failure = assertThrows(MojoExecutionException.class, injection::apply);

    assertEquals(
        "The hotswap directory " + directory + " contains a space, which the container "
            + "cannot pass to Java. Configure webforj.hotswap.directory with a path without spaces",
        failure.getMessage());
  }

  @ParameterizedTest
  @CsvSource(value = {"http://localhost:8888/webforj-bbjship,,true",
      "http://localhost:8888/webforj-bbjship," + SPRING + ",false",
      "http://localhost:8888/webforj-bbjship," + JETTY + ",false", "''," + SPRING + ",false",
      "'',,false"})
  void shouldRunInTheSharedContainerOnlyForBbjProjectWithoutRunner(String url, String runner,
      boolean shared, @TempDir Path tmp) {
    MavenProject project = runner == null ? newProject(tmp) : newProject(tmp, runner);
    HotswapInjection.Builder builder = HotswapInjection.create().setProject(project)
        .setBbjDeploymentUrl(url).setLog(mock(Log.class));

    assertEquals(shared, builder.isSharedContainer());
  }

  private static HotswapInjection newInjection(MavenProject project, Properties userProperties,
      HotswapOptions options, String commandLineValue) {
    return HotswapInjection.create().setProject(project).setUserProperties(userProperties)
        .setOptions(options).setCommandLineValue(commandLineValue).setLog(mock(Log.class)).build();
  }

  private static void givenFramework(MavenProject project) {
    Artifact framework = mock(Artifact.class);
    when(framework.getGroupId()).thenReturn("com.webforj");
    when(framework.getArtifactId()).thenReturn(FRAMEWORK);
    when(framework.getBaseVersion()).thenReturn(VERSION);
    when(project.getArtifacts()).thenReturn(Set.of(framework));
  }

  private static Path observerJar(Path tmp) throws IOException {
    Path jar = tmp.resolve("webforj-hotswap-observer.jar");
    if (!Files.exists(jar)) {
      Files.createFile(jar);
    }

    return jar;
  }

  private static HotswapOptions hotswapAgentOptions(Path tmp) throws IOException {
    Path jar = tmp.resolve("hotswap-agent.jar");
    if (!Files.exists(jar)) {
      Files.createFile(jar);
    }

    return new HotswapOptions().setHotswapAgent(new HotswapAgentOptions().setPath(jar.toFile()));
  }

  private static Path fakeJava(Path dir, int exitCode) throws IOException {
    Assumptions.assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"),
        "the capability check stand in needs a posix file system");

    Path script = dir.resolve("java-" + exitCode);
    Files.writeString(script, "#!/bin/sh\nexit " + exitCode + "\n");
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"));

    return script;
  }

  private static HotswapOptions jrebelOptions(Path tmp) throws IOException {
    Path library = tmp.resolve("libjrebel64.dylib");
    if (!Files.exists(library)) {
      Files.createFile(library);
    }

    return new HotswapOptions().setJrebel(new JrebelOptions().setPath(library.toFile()));
  }

  private static MavenProject newProject(Path tmp, String... runnerArtifactIds) {
    Build build = new Build();
    build.setDirectory(tmp.resolve("target").toString());

    List<Plugin> plugins = new ArrayList<>();
    for (String artifactId : runnerArtifactIds) {
      Plugin plugin = new Plugin();
      plugin.setArtifactId(artifactId);
      plugins.add(plugin);
    }

    Properties properties = new Properties();
    MavenProject project = mock(MavenProject.class);
    when(project.getBuild()).thenReturn(build);
    when(project.getProperties()).thenReturn(properties);
    when(project.getBuildPlugins()).thenReturn(plugins);

    return project;
  }
}
