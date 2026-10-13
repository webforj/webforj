package com.webforj.plugin.foundation.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.hotswap.hotswapagent.HotswapAgentAttachment;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath;
import com.webforj.plugin.foundation.resolve.ApplicationClasspath.ResolvedJar;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class HotswapLaunchTest {

  private static final String VERSION = "26.02-SNAPSHOT";
  private static final String AGENT_JAR = "hotswap-agent.jar";
  private static final String OBSERVER_JAR = "webforj-hotswap-observer.jar";
  private static final String BUILD_FOLDER = "build";
  private static final String HOTSWAP_FOLDER = "hotswap";
  private static final String JAVAAGENT_PREFIX = "-javaagent:";
  private static final String OPTIONS_FILE = "hotswap/hotswap.options";

  @Test
  void shouldStayOffWithoutAnyConfiguration() throws Exception {
    assertTrue(HotswapLaunch.create().build().getArguments().isEmpty());
  }

  @Test
  void shouldStayOffWhenTheCommandLineSaysOff(@TempDir Path tmp) throws Exception {
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true)
        .setJrebelPath(jrebelLibrary(tmp)).setCommandLineValue("off").build();

    assertTrue(launch.getArguments().isEmpty());
  }

  @Test
  void shouldRejectAnUnknownCommandLineValue() {
    HotswapLaunch launch = HotswapLaunch.create().setCommandLineValue("dcevm").build();

    assertThrows(IllegalArgumentException.class, launch::getArguments);
  }

  @Test
  void shouldFailWhenTheBuildNamesBothTools(@TempDir Path tmp) throws Exception {
    HotswapLaunch launch = HotswapLaunch.create().setHotswapAgentConfigured(true)
        .setJrebelConfigured(true).setJrebelPath(jrebelLibrary(tmp)).build();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, launch::getArguments);

    assertTrue(failure.getMessage().contains("hotswapAgent and jrebel"));
  }

  @Test
  void shouldRequireTheJrebelPath() {
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true).build();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, launch::getArguments);

    assertTrue(failure.getMessage().contains("jrebel path"));
  }

  @Test
  void shouldComposeTheJrebelArguments(@TempDir Path tmp) throws Exception {
    Path library = jrebelLibrary(tmp);
    HotswapLaunch launch =
        HotswapLaunch.create().setJrebelConfigured(true).setJrebelPath(library).build();

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath(), "-Dwebforj.hotswap.tool=jrebel",
        "-Dwebforj.hotswap.level=full"), launch.getArguments());
  }

  @Test
  void shouldLogTheCommandLineSelection(@TempDir Path tmp) throws Exception {
    List<String> lines = new ArrayList<>();
    HotswapLaunch.create().setJrebelConfigured(true).setJrebelPath(jrebelLibrary(tmp))
        .setCommandLineValue("jrebel").setLog(lines::add).build().getArguments();

    assertTrue(lines.stream().anyMatch(line -> line.contains("jrebel")),
        "the command line selection is reported");
  }

  @Test
  void shouldComposeTheHotswapAgentArguments(@TempDir Path tmp) throws Exception {
    Path agentJar = Files.createFile(tmp.resolve(AGENT_JAR));
    Path observer = Files.createFile(tmp.resolve(OBSERVER_JAR));
    List<String> versions = new ArrayList<>();

    List<String> arguments = HotswapLaunch.create().setHotswapAgentConfigured(true)
        .setHotswapAgentPath(agentJar).setBuildDirectory(tmp.resolve(BUILD_FOLDER))
        .setDirectory(tmp.resolve(HOTSWAP_FOLDER)).setJavaExecutable(fakeJava(tmp, 0))
        .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
        .setResolver((groupId, artifactId, version) -> {
          versions.add(version);
          return List.of(new ResolvedJar(groupId, artifactId, version, observer));
        }).build().getArguments();

    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"));
    assertTrue(arguments.stream()
        .anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX + agentJar.toAbsolutePath())
            && argument.contains("autoHotswap=true")));
    assertTrue(arguments.contains(JAVAAGENT_PREFIX + observer.toAbsolutePath()),
        "the resolved observer attaches behind the agent");
    assertEquals(List.of(VERSION), versions,
        "the observer resolves at the framework version of the application");
  }

  @Test
  void shouldAttachWithoutTheFlagOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    Path agentJar = Files.createFile(tmp.resolve(AGENT_JAR));
    Path observer = Files.createFile(tmp.resolve(OBSERVER_JAR));

    List<String> arguments =
        HotswapLaunch.create().setHotswapAgentConfigured(true).setHotswapAgentPath(agentJar)
            .setBuildDirectory(tmp.resolve(BUILD_FOLDER)).setDirectory(tmp.resolve(HOTSWAP_FOLDER))
            .setJavaExecutable(fakeJava(tmp, 1))
            .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
            .setResolver((groupId, artifactId, version) -> List
                .of(new ResolvedJar(groupId, artifactId, version, observer)))
            .build().getArguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith(JAVAAGENT_PREFIX)),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals),
        "the unsupported flag never reaches the virtual machine");
  }

  @Test
  void shouldWrapTheObserverResolutionFailure(@TempDir Path tmp) throws Exception {
    Path agentJar = Files.createFile(tmp.resolve(AGENT_JAR));
    HotswapLaunch launch = HotswapLaunch.create().setHotswapAgentConfigured(true)
        .setHotswapAgentPath(agentJar).setBuildDirectory(tmp.resolve(BUILD_FOLDER))
        .setDirectory(tmp.resolve(HOTSWAP_FOLDER)).setJavaExecutable(fakeJava(tmp, 0))
        .setApplicationClasspath(() -> new ApplicationClasspath(List.of()))
        .setResolver((groupId, artifactId, version) -> List.of()).build();

    IOException failure = assertThrows(IOException.class, launch::getArguments);

    assertTrue(failure.getMessage().contains(ApplicationClasspath.FRAMEWORK_ARTIFACT_ID),
        "the missing requirement is named");
  }

  @Test
  void shouldComposeTheSharedContainerArgumentWithoutBuildDirectory(@TempDir Path tmp)
      throws Exception {
    Path agentJar = Files.createFile(tmp.resolve(AGENT_JAR));
    Path observer = Files.createFile(tmp.resolve(OBSERVER_JAR));
    HotswapLaunch launch =
        HotswapLaunch.create().setHotswapAgentConfigured(true).setHotswapAgentPath(agentJar)
            .setDirectory(tmp.resolve(HOTSWAP_FOLDER)).setJavaExecutable(fakeJava(tmp, 0))
            .setSharedContainer(true).setContainerOptionsDirectory(tmp.resolve(HOTSWAP_FOLDER))
            .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
            .setResolver((groupId, artifactId, version) -> List
                .of(new ResolvedJar(groupId, artifactId, version, observer)))
            .build();

    List<String> arguments = launch.getArguments();

    assertEquals(List.of("-XX:VMOptionsFile=" + tmp.resolve(OPTIONS_FILE).toAbsolutePath()),
        arguments);
  }

  @Test
  void shouldWriteTheJrebelArgumentsIntoTheSharedContainerOptions(@TempDir Path tmp)
      throws Exception {
    Path library = jrebelLibrary(tmp);
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true).setJrebelPath(library)
        .setDirectory(tmp.resolve(HOTSWAP_FOLDER)).setSharedContainer(true)
        .setContainerOptionsDirectory(tmp.resolve(HOTSWAP_FOLDER)).build();

    List<String> arguments = launch.getArguments();

    assertEquals(List.of("-XX:VMOptionsFile=" + tmp.resolve(OPTIONS_FILE).toAbsolutePath()),
        arguments);
    assertEquals("""
        -agentpath:%s
        -Dwebforj.hotswap.tool=jrebel
        -Dwebforj.hotswap.level=full
        """.formatted(library.toAbsolutePath()), Files.readString(tmp.resolve(OPTIONS_FILE)));
  }

  @Test
  void shouldRequireTheOptionsDirectoryOfSharedContainer(@TempDir Path tmp) throws Exception {
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true)
        .setJrebelPath(jrebelLibrary(tmp)).setSharedContainer(true).build();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, launch::getArguments);

    assertEquals("the shared container needs the options directory", failure.getMessage());
  }

  @Test
  void shouldGiveTheHotswapFolderOfTheUserHomeAsDefaultDirectory() {
    assertEquals(Path.of(System.getProperty("user.home"), ".webforj", HOTSWAP_FOLDER),
        HotswapLaunch.getDefaultDirectory());
  }

  @Test
  void shouldAttachTheAgentCachedInTheHotswapAgentFolderOfTheDirectory(@TempDir Path tmp)
      throws Exception {
    Path directory = tmp.resolve(HOTSWAP_FOLDER);
    Path versionFolder = Files.createDirectories(
        directory.resolve("hotswap-agent").resolve(HotswapAgentAttachment.DEFAULT_VERSION));
    Path cached = Files.createFile(
        versionFolder.resolve("hotswap-agent-" + HotswapAgentAttachment.DEFAULT_VERSION + ".jar"));
    Path observer = Files.createFile(tmp.resolve(OBSERVER_JAR));

    List<String> arguments =
        HotswapLaunch.create().setHotswapAgentConfigured(true)
            .setBuildDirectory(tmp.resolve(BUILD_FOLDER)).setDirectory(directory)
            .setJavaExecutable(fakeJava(tmp, 0))
            .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
            .setResolver((groupId, artifactId, version) -> List
                .of(new ResolvedJar(groupId, artifactId, version, observer)))
            .build().getArguments();

    assertTrue(
        arguments.stream().anyMatch(
            argument -> argument.startsWith(JAVAAGENT_PREFIX + cached.toAbsolutePath() + "=")),
        "the agent attaches from the hotswap agent folder of the directory");
  }

  @ParameterizedTest
  @ValueSource(strings = {"hotswap directory", "hotswap\tdirectory"})
  void shouldRefuseTheHotswapDirectoryWithWhitespace(String name, @TempDir Path tmp)
      throws Exception {
    Path directory = tmp.resolve(name);
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true)
        .setJrebelPath(jrebelLibrary(tmp)).setDirectory(directory).setSharedContainer(true)
        .setContainerOptionsDirectory(directory).build();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, launch::getArguments);

    assertEquals(
        "The hotswap directory " + directory + " contains a space, which the container "
            + "cannot pass to Java. Configure webforj.hotswap.directory with a path without spaces",
        failure.getMessage());
    assertFalse(Files.exists(directory), "nothing is written into the refused directory");
  }

  @Test
  void shouldAcceptTheHotswapDirectoryWithWhitespaceWhenJrebelRunsOutsideSharedContainer(
      @TempDir Path tmp) throws Exception {
    Path library = jrebelLibrary(tmp);
    Path directory = tmp.resolve("hotswap directory");
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true).setJrebelPath(library)
        .setDirectory(directory).build();

    assertEquals(List.of("-agentpath:" + library.toAbsolutePath(), "-Dwebforj.hotswap.tool=jrebel",
        "-Dwebforj.hotswap.level=full"), launch.getArguments());
  }

  @Test
  void shouldRefuseTheJrebelPathWithWhitespace(@TempDir Path tmp) throws Exception {
    Path library = Files
        .createFile(Files.createDirectories(tmp.resolve("jrebel lib")).resolve("libjrebel.so"));
    Path directory = tmp.resolve(HOTSWAP_FOLDER);
    HotswapLaunch launch = HotswapLaunch.create().setJrebelConfigured(true).setJrebelPath(library)
        .setDirectory(directory).setSharedContainer(true).setContainerOptionsDirectory(directory)
        .build();

    IllegalArgumentException failure =
        assertThrows(IllegalArgumentException.class, launch::getArguments);

    assertEquals(
        "The JRebel library path " + library + " contains a space, which the container "
            + "cannot pass to Java. Configure the JRebel path without spaces",
        failure.getMessage());
    assertFalse(Files.exists(directory), "nothing is written into the hotswap directory");
  }

  private static ApplicationClasspath classpathWithFramework() {
    return new ApplicationClasspath(List.of(new ResolvedJar(ApplicationClasspath.FRAMEWORK_GROUP_ID,
        ApplicationClasspath.FRAMEWORK_ARTIFACT_ID, VERSION, Path.of("webforj-foundation.jar"))));
  }

  private static Path jrebelLibrary(Path tmp) throws IOException {
    Path library = tmp.resolve("libjrebel64.dylib");
    if (!Files.exists(library)) {
      Files.createFile(library);
    }

    return library;
  }

  private static Path fakeJava(Path dir, int exitCode) throws IOException {
    Assumptions.assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"),
        "the capability check stand in needs a posix file system");

    Path script = dir.resolve("java-" + exitCode);
    Files.writeString(script, "#!/bin/sh\nexit " + exitCode + "\n");
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwx------"));

    return script;
  }
}
