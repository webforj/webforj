package com.webforj.plugin.foundation.hotswap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

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

class HotswapLaunchTest {

  private static final String VERSION = "26.02-SNAPSHOT";

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
    Path agentJar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    Path observer = Files.createFile(tmp.resolve("webforj-hotswap-observer.jar"));
    List<String> versions = new ArrayList<>();

    List<String> arguments = HotswapLaunch.create().setHotswapAgentConfigured(true)
        .setHotswapAgentPath(agentJar).setBuildDirectory(tmp.resolve("build"))
        .setAgentCacheRoot(tmp.resolve("cache")).setJavaExecutable(fakeJava(tmp, 0))
        .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
        .setResolver((groupId, artifactId, version) -> {
          versions.add(version);
          return List.of(new ResolvedJar(groupId, artifactId, version, observer));
        }).build().getArguments();

    assertTrue(arguments.contains("-XX:+AllowEnhancedClassRedefinition"));
    assertTrue(arguments.stream()
        .anyMatch(argument -> argument.startsWith("-javaagent:" + agentJar.toAbsolutePath())
            && argument.contains("autoHotswap=true")));
    assertTrue(arguments.contains("-javaagent:" + observer.toAbsolutePath()),
        "the resolved observer attaches behind the agent");
    assertEquals(List.of(VERSION), versions,
        "the observer resolves at the framework version of the application");
  }

  @Test
  void shouldAttachWithoutTheFlagOnTheVirtualMachineWithoutRedefinitionSupport(@TempDir Path tmp)
      throws Exception {
    Path agentJar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    Path observer = Files.createFile(tmp.resolve("webforj-hotswap-observer.jar"));

    List<String> arguments =
        HotswapLaunch.create().setHotswapAgentConfigured(true).setHotswapAgentPath(agentJar)
            .setBuildDirectory(tmp.resolve("build")).setAgentCacheRoot(tmp.resolve("cache"))
            .setJavaExecutable(fakeJava(tmp, 1))
            .setApplicationClasspath(HotswapLaunchTest::classpathWithFramework)
            .setResolver((groupId, artifactId, version) -> List
                .of(new ResolvedJar(groupId, artifactId, version, observer)))
            .build().getArguments();

    assertTrue(arguments.stream().anyMatch(argument -> argument.startsWith("-javaagent:")),
        "the agent still attaches for the method body changes");
    assertTrue(arguments.stream().noneMatch("-XX:+AllowEnhancedClassRedefinition"::equals),
        "the unsupported flag never reaches the virtual machine");
  }

  @Test
  void shouldWrapTheObserverResolutionFailure(@TempDir Path tmp) throws Exception {
    Path agentJar = Files.createFile(tmp.resolve("hotswap-agent.jar"));
    HotswapLaunch launch = HotswapLaunch.create().setHotswapAgentConfigured(true)
        .setHotswapAgentPath(agentJar).setBuildDirectory(tmp.resolve("build"))
        .setAgentCacheRoot(tmp.resolve("cache")).setJavaExecutable(fakeJava(tmp, 0))
        .setApplicationClasspath(() -> new ApplicationClasspath(List.of()))
        .setResolver((groupId, artifactId, version) -> List.of()).build();

    IOException failure = assertThrows(IOException.class, launch::getArguments);

    assertTrue(failure.getMessage().contains(ApplicationClasspath.FRAMEWORK_ARTIFACT_ID),
        "the missing requirement is named");
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
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"));

    return script;
  }
}
