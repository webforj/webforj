package com.webforj.plugin.foundation.bbj;

import static com.webforj.plugin.foundation.bbj.ServicesProperties.JAVA_ARGUMENTS_KEY;
import static com.webforj.plugin.foundation.bbj.ServicesProperties.JAVA_EXECUTABLE_KEY;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class ServicesPropertiesTest {

  private static final String SERVICES_PROPERTIES = """
      #BBj Services Properties
      basis.cacheDirectory=/opt/bbx/cache
      basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
      basis.java.args.Default=-Xmx512m
      basis.java.jvm.BBjServices=/opt/java/bin/java
      basis.java.jvm.Default=/opt/java/bin/java
      """;
  private static final String OPTIONS_ARGUMENT = "-XX:VMOptionsFile=/opt/hotswap/hotswap.options";
  private static final String PROPERTIES_FILE = "BBj.properties";
  private static final String JAVA_EXECUTABLE = "/opt/java/bin/java";
  private static final String ENCODING_ARGUMENT = "-Dfile.encoding=UTF-8";
  private static final String MEMORY_ARGUMENT = "-Xmx1024m";
  private static final String BACKUP_FILE = "BBj.properties.before-webforj";
  private static final String JBR_EXECUTABLE = "/opt/jbr/bin/java";

  @TempDir
  Path temporary;

  private Path runtime;

  @BeforeEach
  void initRuntimeFolder() throws IOException {
    runtime = Files.createDirectories(temporary.resolve("plugins/.webforj-bbjship-deployments"));
  }

  @Test
  void shouldReadTheJavaSettingsOfBbjServices() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);

    ServicesProperties properties = ServicesProperties.load(file, runtime);

    assertEquals(Optional.of(Path.of(JAVA_EXECUTABLE)), properties.getJavaExecutable());
    assertEquals(List.of(MEMORY_ARGUMENT, ENCODING_ARGUMENT), properties.getJavaArguments());
  }

  @Test
  void shouldAnswerNoJavaSettingsWhenTheKeysAreAbsent() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, "basis.cacheDirectory=/opt/bbx/cache\n");

    ServicesProperties properties = ServicesProperties.load(file, runtime);

    assertEquals(Optional.empty(), properties.getJavaExecutable());
    assertEquals(Optional.empty(), properties.getDefaultJavaExecutable());
    assertEquals(List.of(), properties.getJavaArguments());
  }

  @Test
  void shouldReadTheDefaultJavaExecutableOfBbj() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        basis.java.jvm.Default=/opt/zulu/bin/java
        """);

    ServicesProperties properties = ServicesProperties.load(file, runtime);

    assertEquals(Optional.of(Path.of("/opt/zulu/bin/java")), properties.getDefaultJavaExecutable());
    assertEquals(Optional.empty(), properties.getJavaExecutable());
  }

  @Test
  void shouldRefuseMissingFile() {
    Path file = temporary.resolve("missing.properties");

    IOException failure =
        assertThrows(IOException.class, () -> ServicesProperties.load(file, runtime));

    assertEquals("BBjServices properties file not found at " + file + ". Configure bbj.properties",
        failure.getMessage());
  }

  @ParameterizedTest
  @MethodSource("getJavaSettings")
  void shouldTellWhetherTheJavaSettingsArePresent(String executable, String arguments,
      boolean present) throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        basis.java.jvm.BBjServices=%s
        basis.java.args.BBjServices=%s
        """.formatted(executable, arguments));
    ServicesProperties properties = ServicesProperties.load(file, runtime);

    boolean result = properties.hasJavaSettings(OPTIONS_ARGUMENT);

    assertEquals(present, result);
  }

  @Test
  void shouldApplyTheJavaSettingsAndKeepEveryOtherKeyAndArgument() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);
    final byte[] original = Files.readAllBytes(file);
    ServicesProperties properties = ServicesProperties.load(file, runtime);

    properties.applyJavaSettings(OPTIONS_ARGUMENT);

    Properties written = new Properties();
    try (InputStream input = Files.newInputStream(file)) {
      written.load(input);
    }

    assertEquals(List.of("basis.java.jvm.BBjServices=/opt/java/bin/java"), Files.readAllLines(file)
        .stream().filter(line -> line.startsWith("basis.java.jvm.BBjServices=")).toList());
    assertEquals("-Xmx1024m -Dfile.encoding=UTF-8 " + OPTIONS_ARGUMENT,
        written.getProperty("basis.java.args.BBjServices"));
    assertEquals("/opt/bbx/cache", written.getProperty("basis.cacheDirectory"));
    assertEquals("-Xmx512m", written.getProperty("basis.java.args.Default"));
    assertEquals(JAVA_EXECUTABLE, written.getProperty("basis.java.jvm.Default"));
    assertEquals(5, written.size());
    assertEquals(Optional.of(Path.of(JAVA_EXECUTABLE)), properties.getJavaExecutable());
    assertArrayEquals(original,
        Files.readAllBytes(runtime.resolve(PROPERTIES_FILE + ServicesProperties.BACKUP_SUFFIX)));
    assertFalse(
        Files.exists(temporary.resolve(PROPERTIES_FILE + ServicesProperties.BACKUP_SUFFIX)));
  }

  @Test
  void shouldReplaceTheOptionsFileArgumentInItsPlace() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        basis.java.jvm.BBjServices=/opt/jbr/bin/java
        basis.java.args.BBjServices=-Xmx1g -XX:VMOptionsFile=/old/x.options -Xss1m
        """);
    ServicesProperties properties = ServicesProperties.load(file, runtime);

    properties.applyJavaSettings(OPTIONS_ARGUMENT);

    assertEquals(List.of("-Xmx1g", OPTIONS_ARGUMENT, "-Xss1m"),
        ServicesProperties.load(file, runtime).getJavaArguments());
  }

  @Test
  void shouldWriteTheBackupOnlyOnce() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);
    final byte[] original = Files.readAllBytes(file);
    ServicesProperties.load(file, runtime).applyJavaSettings(OPTIONS_ARGUMENT);

    ServicesProperties.load(file, runtime)
        .applyJavaSettings("-XX:VMOptionsFile=/opt/hotswap-next/hotswap.options");

    assertEquals(
        List.of(MEMORY_ARGUMENT, ENCODING_ARGUMENT,
            "-XX:VMOptionsFile=/opt/hotswap-next/hotswap.options"),
        ServicesProperties.load(file, runtime).getJavaArguments());
    assertArrayEquals(original, Files.readAllBytes(runtime.resolve(BACKUP_FILE)));
  }

  @Test
  void shouldCreateTheBackupFolderWhenItDoesNotExist() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);
    byte[] original = Files.readAllBytes(file);
    Path backupDirectory = temporary.resolve("bbx/plugins/.webforj-bbjship-deployments");
    ServicesProperties properties = ServicesProperties.load(file, backupDirectory);

    properties.applyJavaSettings(OPTIONS_ARGUMENT);

    assertArrayEquals(original, Files.readAllBytes(backupDirectory.resolve(BACKUP_FILE)));
    assertEquals(List.of(MEMORY_ARGUMENT, ENCODING_ARGUMENT, OPTIONS_ARGUMENT),
        ServicesProperties.load(file, backupDirectory).getJavaArguments());
  }

  @Test
  void shouldWriteNothingWhenTheJavaSettingsArePresent() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        basis.java.jvm.BBjServices=/opt/jbr/bin/java
        basis.java.args.BBjServices=-Xmx1024m %s
        """.formatted(OPTIONS_ARGUMENT));
    byte[] original = Files.readAllBytes(file);
    ServicesProperties properties = ServicesProperties.load(file, runtime);

    properties.applyJavaSettings(OPTIONS_ARGUMENT);

    assertArrayEquals(original, Files.readAllBytes(file));
    assertFalse(Files.exists(runtime.resolve(BACKUP_FILE)));
    assertTrue(properties.hasJavaSettings(OPTIONS_ARGUMENT));
  }

  @Test
  void shouldRestoreTheBackupAndRemoveIt() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);
    byte[] original = Files.readAllBytes(file);
    ServicesProperties.load(file, runtime).applyJavaSettings(OPTIONS_ARGUMENT);

    boolean restored = ServicesProperties.resetJavaSettings(file, runtime);

    assertTrue(restored);
    assertArrayEquals(original, Files.readAllBytes(file));
    assertFalse(Files.exists(runtime.resolve(BACKUP_FILE)));
  }

  @Test
  void shouldLeaveTheFileWhenNoBackupExists() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, SERVICES_PROPERTIES);
    byte[] original = Files.readAllBytes(file);

    boolean restored = ServicesProperties.resetJavaSettings(file, runtime);

    assertFalse(restored);
    assertArrayEquals(original, Files.readAllBytes(file));
  }

  @Test
  void shouldRemoveTheHotswapArgumentWhenNoBackupExists() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        %s=-Xmx1024m %s -Dfile.encoding=UTF-8
        %s=/opt/java/bin/java
        """.formatted(JAVA_ARGUMENTS_KEY, OPTIONS_ARGUMENT, JAVA_EXECUTABLE_KEY));

    boolean reset = ServicesProperties.resetJavaSettings(file, runtime);

    assertTrue(reset);
    ServicesProperties properties = ServicesProperties.load(file, runtime);
    assertEquals(List.of(MEMORY_ARGUMENT, ENCODING_ARGUMENT), properties.getJavaArguments());
    assertEquals(Optional.of(Path.of(JAVA_EXECUTABLE)), properties.getJavaExecutable());
    assertFalse(properties.hasHotswapArgument());
  }

  @Test
  void shouldKeepTheOptionsFileArgumentOfAnotherFileWhenNoBackupExists() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        %s=-Xmx1024m -XX:VMOptionsFile=/opt/app/jvm.options
        """.formatted(JAVA_ARGUMENTS_KEY));
    byte[] original = Files.readAllBytes(file);

    boolean reset = ServicesProperties.resetJavaSettings(file, runtime);

    assertFalse(reset);
    assertArrayEquals(original, Files.readAllBytes(file));
    assertFalse(ServicesProperties.load(file, runtime).hasHotswapArgument());
  }

  @Test
  void shouldFindTheHotswapArgumentOfBbjShip() throws IOException {
    Path file = temporary.resolve(PROPERTIES_FILE);
    Files.writeString(file, """
        %s=-Xmx1024m %s
        """.formatted(JAVA_ARGUMENTS_KEY, OPTIONS_ARGUMENT));

    ServicesProperties properties = ServicesProperties.load(file, runtime);

    assertTrue(properties.hasHotswapArgument());
  }

  @ParameterizedTest
  @MethodSource("getJavaExecutables")
  void shouldFindTheJavaOfBbjServicesInThePropertiesFileOfTheHome(String properties,
      Optional<Path> java) throws IOException {
    Path bbx = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(bbx.resolve("cfg/BBj.properties"), properties);

    assertEquals(java, ServicesProperties.findJavaExecutable(bbx, null));
  }

  @Test
  void shouldFindTheJavaOfBbjServicesInTheConfiguredPropertiesFile() throws IOException {
    Path bbx = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(bbx.resolve("cfg/BBj.properties"), """
        basis.java.jvm.BBjServices=/opt/zulu/bin/java
        """);
    Path file = Files.writeString(temporary.resolve(PROPERTIES_FILE), """
        basis.java.jvm.BBjServices=/opt/jbr/bin/java
        """);

    assertEquals(Optional.of(Path.of(JBR_EXECUTABLE)),
        ServicesProperties.findJavaExecutable(bbx, file));
  }

  @Test
  void shouldFindJavaExecutableInNamedFileWithoutHome() throws IOException {
    Path file = Files.writeString(temporary.resolve(PROPERTIES_FILE), """
        basis.java.jvm.BBjServices=/opt/jbr/bin/java
        """);

    assertEquals(Optional.of(Path.of(JBR_EXECUTABLE)),
        ServicesProperties.findJavaExecutable(null, file));
  }

  @Test
  void shouldFindNoJavaWithoutThePropertiesFile() throws IOException {
    Path bbx = Home.createCopy(temporary.resolve("bbx"));

    assertEquals(Optional.empty(), ServicesProperties.findJavaExecutable(bbx, null));
  }

  @Test
  void shouldRefuseHomeThatIsNoBbjInstallation() {
    Path home = temporary.resolve("bbx");

    IOException failure =
        assertThrows(IOException.class, () -> ServicesProperties.findJavaExecutable(home, null));

    assertEquals("Invalid BBj home. Configure a local BBj installation", failure.getMessage());
  }

  private static Stream<Arguments> getJavaExecutables() {
    return Stream.of(Arguments.of("""
        basis.java.jvm.BBjServices=/opt/jbr/bin/java
        basis.java.jvm.Default=/opt/zulu/bin/java
        """, Optional.of(Path.of(JBR_EXECUTABLE))), Arguments.of("""
        basis.java.jvm.Default=/opt/zulu/bin/java
        """, Optional.of(Path.of("/opt/zulu/bin/java"))), Arguments.of("""
        basis.java.jvm.BBjServices=/jdk/jbr/bin/server/jvm.dll
        basis.java.jvm.Default=/jdk/zulu/bin/server/jvm.dll
        """, Optional.of(Path.of("/jdk/jbr/bin/java.exe"))), Arguments.of("""
        basis.java.jvm.Default=/jdk/zulu/bin/client/JVM.DLL
        """, Optional.of(Path.of("/jdk/zulu/bin/java.exe"))), Arguments.of("""
        basis.java.jvm.Default=jvm.dll
        """, Optional.of(Path.of("jvm.dll"))), Arguments.of("""
        basis.cacheDirectory=/opt/bbx/cache
        """, Optional.empty()));
  }

  private static Stream<Arguments> getJavaSettings() {
    return Stream.of(Arguments.of(JBR_EXECUTABLE, "-Xmx1024m " + OPTIONS_ARGUMENT, true),
        Arguments.of(JAVA_EXECUTABLE, "-Xmx1024m " + OPTIONS_ARGUMENT, true),
        Arguments.of(JBR_EXECUTABLE, MEMORY_ARGUMENT, false));
  }
}
