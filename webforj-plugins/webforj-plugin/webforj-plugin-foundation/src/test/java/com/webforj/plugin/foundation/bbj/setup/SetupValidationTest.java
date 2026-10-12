package com.webforj.plugin.foundation.bbj.setup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import com.webforj.plugin.foundation.bbj.Home;
import com.webforj.plugin.foundation.bbj.Setup;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class SetupValidationTest {

  @TempDir
  Path temporary;

  @ParameterizedTest
  @ValueSource(strings = {"plugins", "plugins/webforj-bbjship"})
  void shouldRefuseFileInPlaceOfThePluginFolder(String file) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.createDirectories(home.resolve(file).getParent());
    Files.writeString(home.resolve(file), "not a folder");
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj plugin destination must be a directory without symbolic links",
        failure.getMessage());
    assertEquals("not a folder", Files.readString(home.resolve(file)));
    assertFalse(Files.exists(home.resolve("invocations")));
  }

  @ParameterizedTest
  @ValueSource(
      strings = {"plugins", "plugins/webforj-bbjship", "plugins/.webforj-bbjship-deployments"})
  void shouldRefuseSymbolicLinkInPlaceOfThePluginFolder(String link) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path outside = Files.createDirectory(temporary.resolve("outside"));
    Files.createDirectories(home.resolve(link).getParent());
    Files.createSymbolicLink(home.resolve(link), outside);
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj plugin destination must be a directory without symbolic links",
        failure.getMessage());
    assertEquals(0, outside.toFile().list().length);
    assertFalse(Files.exists(home.resolve("invocations")));
  }

  @Test
  void shouldRefuseSymbolicLinkInsideTheInstalledBbjShipEvenWithForce() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();
    setup.apply();
    Path outside = Files.writeString(temporary.resolve("outside.properties"), "kept=true");
    Path link = home.resolve("plugins/webforj-bbjship/external.properties");
    Files.createSymbolicLink(link, outside);
    String invocations = Files.readString(home.resolve("invocations"));

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj plugin destination must not contain symbolic links", failure.getMessage());
    assertEquals(invocations, Files.readString(home.resolve("invocations")));
    assertEquals("kept=true", Files.readString(outside));
    assertTrue(Files.isSymbolicLink(link));
  }

  @Test
  void shouldRefuseNullPluginZipOrVersion() {
    Path zip = temporary.resolve("webforj-bbjship.zip");
    String version = "26.03";
    Path missingZip = null;
    String missingVersion = null;

    assertThrows(NullPointerException.class, () -> Setup.create(missingZip, version));
    assertThrows(NullPointerException.class, () -> Setup.create(zip, missingVersion));
  }

  @Test
  void shouldRefuseTokenCredentialsBeforeWriting() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.ofToken("4f2a9c")).setPort(8888).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup requires the administration password. Configure bbj.admin.password "
        + "instead of a token", failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @ParameterizedTest
  @ValueSource(ints = {0, 65536})
  void shouldRefusePortOutsideTheRangeBeforeWriting(int port) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(port)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup requires a web server port from 1 to 65535", failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @ParameterizedTest
  @ValueSource(strings = {"ftp://localhost/webforj-bbjship", "http:/webforj-bbjship",
      "http://administrator:secret@localhost/webforj-bbjship"})
  void shouldRefuseDeploymentUrlThatIsNotPlainHttpOrHttpsBeforeWriting(String url)
      throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(URI.create(url))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup requires a local HTTP or HTTPS deployment URL", failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @Test
  void shouldRefuseDeploymentUrlOfAnotherMachineEvenWithForce() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://192.0.2.1:8888/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup is local only. Run setup on the BBj host", failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @Test
  void shouldNameDeploymentUrlHostThatCannotBeResolved() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://unresolvable.invalid:8888/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup cannot resolve the deployment URL host unresolvable.invalid. "
        + "Use localhost or a name of this machine", failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @ParameterizedTest
  @MethodSource("getIncompleteHomes")
  void shouldRefuseBbjHomeWithoutConfigurationOrInterpreter(String deleted, String message)
      throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.delete(home.resolve(deleted));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(message, failure.getMessage());
    assertEquals(Set.of("bin", "cfg"), Set.of(home.toFile().list()));
  }

  @Test
  void shouldResolveTheConfiguredHomeToItsRealPath() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    Path resolved = Setup.resolveHome(home.resolve("cfg/.."));

    assertEquals(home.toRealPath(), resolved);
  }

  @ParameterizedTest
  @MethodSource("getUnsafeEntries")
  void shouldRefuseUnsafeZipEntryBeforeInstalling(String name, String message) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    Path zip = temporary.resolve("webforj-bbjship.zip");
    try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(zip))) {
      output.putNextEntry(new ZipEntry(name));
      output.write("escaped".getBytes(StandardCharsets.UTF_8));
    }

    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(message, failure.getMessage());
    assertEquals(Set.of("bin", "cfg", "plugins"), Set.of(home.toFile().list()));
    assertEquals(Set.of(".webforj-bbjship-deployments"),
        Set.of(home.resolve("plugins").toFile().list()));
    assertEquals(Set.of(),
        Set.of(home.resolve("plugins/.webforj-bbjship-deployments").toFile().list()));
    assertFalse(Files.exists(temporary.resolve("escaped")));
  }

  @Test
  void shouldRefuseDuplicateZipEntryBeforeInstalling() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));

    Path zip = temporary.resolve("webforj-bbjship.zip");
    try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(zip))) {
      output.putNextEntry(new ZipEntry("cli.bbj"));
      output.putNextEntry(new ZipEntry("cli.bbX"));
    }

    String content = Files.readString(zip, StandardCharsets.ISO_8859_1);
    Files.writeString(zip, content.replace("cli.bbX", "cli.bbj"), StandardCharsets.ISO_8859_1);
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj deployment archive has duplicate or excessive entries", failure.getMessage());
    assertEquals(Set.of("bin", "cfg", "plugins"), Set.of(home.toFile().list()));
    assertEquals(Set.of(".webforj-bbjship-deployments"),
        Set.of(home.resolve("plugins").toFile().list()));
    assertEquals(Set.of(),
        Set.of(home.resolve("plugins/.webforj-bbjship-deployments").toFile().list()));
  }

  @ParameterizedTest
  @ValueSource(strings = {"bbj-plugin.json", "version.properties", "bbjship"})
  void shouldRefuseBbjShipPackageWithoutRequiredFile(String required) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Files.delete(plugin.resolve(required));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBjShip package is incomplete. Missing " + required, failure.getMessage());
    assertEquals(Set.of("bin", "cfg", "plugins"), Set.of(home.toFile().list()));
    assertEquals(Set.of(".webforj-bbjship-deployments"),
        Set.of(home.resolve("plugins").toFile().list()));
    assertEquals(Set.of(),
        Set.of(home.resolve("plugins/.webforj-bbjship-deployments").toFile().list()));
  }

  @ParameterizedTest
  @MethodSource("getUnmatchedPackageVersions")
  void shouldRefuseBbjShipPackageOfAnotherVersionEvenWithForce(String versionProperties,
      String message) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Files.writeString(plugin.resolve("version.properties"), versionProperties);
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(message, failure.getMessage());
    assertEquals(Set.of("bin", "cfg", "plugins"), Set.of(home.toFile().list()));
    assertEquals(Set.of(".webforj-bbjship-deployments"),
        Set.of(home.resolve("plugins").toFile().list()));
    assertEquals(Set.of(),
        Set.of(home.resolve("plugins/.webforj-bbjship-deployments").toFile().list()));
  }

  private static Stream<Arguments> getIncompleteHomes() {
    return Stream.of(
        Arguments.of("cfg/config.bbx", "Invalid BBj home. Configure a local BBj installation"),
        Arguments.of("bin/bbj", "BBj executable is missing or not executable. Check bbj.home"));
  }

  private static Stream<Arguments> getUnsafeEntries() {
    String invalid = "Invalid BBj plugin archive entry: ";
    String runtime = "BBj plugin archive must not contain runtime files";
    return Stream.of(Arguments.of("", invalid), Arguments.of("/escaped", invalid + "/escaped"),
        Arguments.of("commands\\escaped", invalid + "commands\\escaped"),
        Arguments.of("C:/escaped", invalid + "C:/escaped"),
        Arguments.of("commands/../../escaped", invalid + "commands/../../escaped"),
        Arguments.of("commands/./escaped", invalid + "commands/./escaped"),
        Arguments.of("commands//escaped", invalid + "commands//escaped"),
        Arguments.of("deployments/hello-app", runtime), Arguments.of("htdocs/index.html", runtime),
        Arguments.of("assets/app.css", runtime), Arguments.of("development.enabled", runtime),
        Arguments.of(".webforj-bbjship-deployments/", runtime));
  }

  private static Stream<Arguments> getUnmatchedPackageVersions() {
    return Stream.of(
        Arguments.of("version=26.02\n", "BBjShip package does not match webforJ 26.03"),
        Arguments.of("version=\n", "BBjShip version is missing in version.properties"));
  }
}
