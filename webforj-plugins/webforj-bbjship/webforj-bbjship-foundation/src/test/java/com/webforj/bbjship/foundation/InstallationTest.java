package com.webforj.bbjship.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

class InstallationTest {

  private static final String MISSING_HOME = "missing";

  @TempDir
  Path temporary;

  @Test
  void shouldResolveTheConfiguredHomeToItsRealPath() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    Path resolved = Installation.resolveHome(home.resolve("cfg/.."));

    assertEquals(home.toRealPath(), resolved);
  }

  @Test
  void shouldRecognizeBbjHomeByItsConfigurationAndExecutable() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    assertTrue(Installation.isHome(home));
    assertFalse(Installation.isHome(temporary.resolve(MISSING_HOME)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"cfg/config.bbx", "bin/bbj"})
  void shouldNotRecognizeBbjHomeWithoutInstallationFile(String file) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.delete(home.resolve(file));

    assertFalse(Installation.isHome(home));
  }

  @Test
  void shouldFindTheInterpreterOfBbjHome() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    assertEquals(Optional.of(home.resolve("bin/bbj")), Installation.findExecutable(home));
  }

  @Test
  void shouldNotFindInterpreterThatIsNotExecutable() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    assertTrue(home.resolve("bin/bbj").toFile().setExecutable(false));

    assertEquals(Optional.empty(), Installation.findExecutable(home));
    assertTrue(Installation.hasConfiguration(home));
    assertFalse(Installation.isHome(home));
  }

  @ParameterizedTest
  @ValueSource(strings = {"cfg/config.bbx", "cfg"})
  void shouldNotFindConfigurationWhenItIsDeleted(String deleted) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.delete(home.resolve("cfg/config.bbx"));
    Files.deleteIfExists(home.resolve(deleted));

    assertFalse(Installation.hasConfiguration(home));
    assertFalse(Installation.hasConfiguration(temporary.resolve(MISSING_HOME)));
  }

  @Test
  void shouldFindBbjHomeOfRunningService() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Process service =
        new ProcessBuilder("/bin/sh", "-c", "sleep 30", "sh", "-Dbasis.BBjHome=" + home).start();
    try {
      Optional<Path> found =
          Installation.findInProcesses(Stream.of(ProcessHandle.of(service.pid()).orElseThrow()));

      assertEquals(Optional.of(home), found);
    } finally {
      service.destroyForcibly().waitFor();
    }
  }

  @Test
  void shouldSkipServiceThatNamesNoBbjHome() throws Exception {
    Process service = new ProcessBuilder("/bin/sh", "-c", "sleep 30", "sh",
        "-Dbasis.BBjHome=" + temporary.resolve(MISSING_HOME)).start();
    try {
      Optional<Path> found =
          Installation.findInProcesses(Stream.of(ProcessHandle.of(service.pid()).orElseThrow()));

      assertEquals(Optional.empty(), found);
      assertEquals(Optional.empty(), Installation.findInProcesses(Stream.empty()));
    } finally {
      service.destroyForcibly().waitFor();
    }
  }

  @Test
  void shouldFindBbjHomeTheInstallerRecorded() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path user = Files.createDirectories(temporary.resolve("user/bbj")).getParent();
    Files.writeString(user.resolve("bbj/Install.properties"), """
        AutoRecordInstallResponse=true
        InstallDirectory=%s
        """.formatted(home));

    assertEquals(Optional.of(home), Installation.findInInstallRecord(user));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "AutoRecordInstallResponse=true\n", "InstallDirectory=\n",
      "InstallDirectory=missing/bbx\n"})
  void shouldSkipInstallRecordThatNamesNoBbjHome(String content) throws IOException {
    Path user = Files.createDirectories(temporary.resolve("user/bbj")).getParent();
    Files.writeString(user.resolve("bbj/Install.properties"), content);

    assertEquals(Optional.empty(), Installation.findInInstallRecord(user));
    assertEquals(Optional.empty(), Installation.findInInstallRecord(temporary.resolve("nobody")));
  }

  @Test
  void shouldFindBbjHomeOfInterpreterOnSearchPath() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path other = Files.createDirectory(temporary.resolve("other"));
    String path = other + File.pathSeparator + File.pathSeparator + home.resolve("bin");

    assertEquals(Optional.of(home), Installation.findOnPath(path));
  }

  @Test
  void shouldSkipSearchPathWhoseInterpreterHasNoConfiguration() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.delete(home.resolve("cfg/config.bbx"));

    assertEquals(Optional.empty(), Installation.findOnPath(home.resolve("bin").toString()));
    assertEquals(Optional.empty(), Installation.findOnPath(null));
  }

  @ParameterizedTest
  @CsvSource({"Mac OS X, Applications/bbx", "Windows 11, bbx", "Linux, usr/local/bbx"})
  void shouldFindBbjHomeInDefaultFolderOfOperatingSystem(String osName, String folder)
      throws IOException {
    Path root = temporary.toRealPath();
    Path home = Home.createCopy(root.resolve(folder));

    assertEquals(Optional.of(home), Installation.findDefault(osName, root));
    assertEquals(Optional.empty(), Installation.findDefault(osName, root.resolve("elsewhere")));
  }
}
