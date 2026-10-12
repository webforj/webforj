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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class SetupInstallationTest {

  @TempDir
  Path temporary;

  @Test
  void shouldInstallBbjShipAndRunRemoteEnableWithPasswordOnStandardInput() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setLog(messages::add).build();

    setup.apply();

    Path installed = home.resolve("plugins/webforj-bbjship");
    try (Stream<Path> files = Files.walk(installed)) {
      assertEquals(
          List.of("bbj-plugin.json", "bbjship", "lib/webforj-bbjship.jar", "version.properties"),
          files.filter(Files::isRegularFile).map(file -> installed.relativize(file).toString())
              .sorted().toList());
    }

    assertTrue(Files.isExecutable(installed.resolve("bbjship")));
    assertEquals("""
        remote
        enable
        --username
        administrator
        --port
        8888
        --password-stdin
        END
        """, Files.readString(home.resolve("invocations")));
    assertEquals("""
        secret-password
        """, Files.readString(home.resolve("stdin")));
    assertEquals("", Files.readString(home.resolve("workspace-properties")));
    assertTrue(Files.exists(home.resolve("remote-enabled")));
    assertEquals(List.of("Using BBj home " + home, "Installing BBjShip 26.03",
        "Enabling BBj remote deployment", "BBj setup completed"), messages);
  }

  @Test
  void shouldRunRemoteEnableOnTheSslPortOfHttpsDeploymentUrl() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("https://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8443)
        .build();

    setup.apply();

    assertEquals("""
        remote
        enable
        --username
        administrator
        --ssl-port
        8443
        --password-stdin
        END
        """, Files.readString(home.resolve("invocations")));
  }

  @Test
  void shouldRemoveFreshBbjShipWhenRemoteEnableFails() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Files.writeString(home.resolve("failure"), "remote");
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals("BBj setup command failed. Check credentials and local BBjServices.",
        failure.getMessage());
    assertEquals("""
        remote
        enable
        --username
        administrator
        --port
        8888
        --password-stdin
        END
        """, Files.readString(home.resolve("invocations")));
    assertEquals("""
        secret-password
        """, Files.readString(home.resolve("stdin")));
    assertFalse(Files.exists(home.resolve("remote-enabled")));
    assertFalse(Files.exists(home.resolve("plugins/webforj-bbjship")));
  }

  @Test
  void shouldRefuseInstalledBbjShipOfAnotherVersionWithoutForce() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship-26.02"));
    Files.writeString(previous.resolve("version.properties"), "version=26.02\n");
    Setup.create(BBjShipPlugin.writeZip(previous, temporary.resolve("previous.zip")), "26.02")
        .setHome(home).setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888).build()
        .apply();
    String invocations = Files.readString(home.resolve("invocations"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(
        "Installed BBjShip version 26.02 does not match 26.03. "
            + "Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace it",
        failure.getMessage());
    assertEquals("version=26.02\n",
        Files.readString(home.resolve("plugins/webforj-bbjship/version.properties")));
    assertEquals(invocations, Files.readString(home.resolve("invocations")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"version=\n", ""})
  void shouldRefuseInstalledBbjShipWithoutVersionWithoutForce(String installedVersion)
      throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .build();
    setup.apply();
    Files.writeString(home.resolve("plugins/webforj-bbjship/version.properties"), installedVersion);
    String invocations = Files.readString(home.resolve("invocations"));

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(
        "Installed BBjShip version is missing. "
            + "Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace the package",
        failure.getMessage());
    assertEquals(installedVersion,
        Files.readString(home.resolve("plugins/webforj-bbjship/version.properties")));
    assertEquals(invocations, Files.readString(home.resolve("invocations")));
  }

  @ParameterizedTest
  @MethodSource("getInstalledBBjShips")
  void shouldReplaceInstalledBbjShipWithForceAndLeaveTheRuntimeFolderUntouched(String version,
      String installedVersion) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship-" + version));
    Files.writeString(previous.resolve("version.properties"), "version=" + version + "\n");
    Setup.create(BBjShipPlugin.writeZip(previous, temporary.resolve("previous.zip")), version)
        .setHome(home).setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888).build()
        .apply();
    Path installed = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    Files.writeString(installed.resolve("version.properties"), installedVersion);
    Files.writeString(installed.resolve("bbj-plugin.json"), "{}");
    Files.createDirectories(installed.resolve("deployments/hello-app"));
    Files.writeString(installed.resolve("deployments/hello-app/application.jar"), "hello-app");
    Files.createDirectories(installed.resolve("htdocs/hello-app"));
    Files.writeString(installed.resolve("htdocs/hello-app/index.html"), "<html></html>");
    Files.createDirectories(installed.resolve("assets/hello-app"));
    Files.writeString(installed.resolve("assets/hello-app/app.css"), ":root {}");
    Files.createDirectories(installed.resolve("commands"));
    Files.writeString(installed.resolve("commands/Removed.bbj"), "rem removed");
    Files.writeString(installed.resolve("lib/removed.jar"), "removed");
    Files.createDirectories(installed.resolve("configuration"));
    Files.writeString(installed.resolve("configuration/local.properties"), "removed=true");
    Files.createDirectories(runtime.resolve("deployments/hello-app"));
    Files.writeString(runtime.resolve("deployments/hello-app/application.jar"), "hello-app");
    Files.createDirectories(runtime.resolve("htdocs"));
    Files.createDirectories(runtime.resolve("assets"));
    Files.createDirectories(home.resolve("plugins/other"));
    Files.writeString(home.resolve("plugins/other/plugin.bbj"), "rem other plugin");
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    setup.apply();

    assertEquals("version=26.03\n", Files.readString(installed.resolve("version.properties")));
    assertEquals("", Files.readString(installed.resolve("bbj-plugin.json")));
    try (Stream<Path> files = Files.walk(installed)) {
      assertEquals(
          List.of("bbj-plugin.json", "bbjship", "lib/webforj-bbjship.jar", "version.properties"),
          files.filter(Files::isRegularFile).map(file -> installed.relativize(file).toString())
              .sorted().toList());
    }

    try (Stream<Path> files = Files.walk(runtime)) {
      assertEquals(
          List.of("", "assets", "deployments", "deployments/hello-app",
              "deployments/hello-app/application.jar", "htdocs"),
          files.map(file -> runtime.relativize(file).toString()).sorted().toList());
    }

    assertEquals("hello-app",
        Files.readString(runtime.resolve("deployments/hello-app/application.jar")));
    assertEquals("rem other plugin", Files.readString(home.resolve("plugins/other/plugin.bbj")));
  }

  @Test
  void shouldKeepDevelopmentSwitchWhenForceReplacesInstalledBbjShip() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship-26.02"));
    Files.writeString(previous.resolve("version.properties"), "version=26.02\n");
    Setup.create(BBjShipPlugin.writeZip(previous, temporary.resolve("previous.zip")), "26.02")
        .setHome(home).setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888).build()
        .apply();
    Path installed = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    Files.writeString(runtime.resolve("development.enabled"),
        "development runs are enabled on this machine");
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    setup.apply();

    assertEquals("version=26.03\n", Files.readString(installed.resolve("version.properties")));
    assertEquals("development runs are enabled on this machine",
        Files.readString(runtime.resolve("development.enabled")));
    assertFalse(Files.exists(installed.resolve("development.enabled")));
  }

  @Test
  void shouldNotCreateDevelopmentSwitchWhenForceReplacesBbjShipWithoutOne() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship-26.02"));
    Files.writeString(previous.resolve("version.properties"), "version=26.02\n");
    Setup.create(BBjShipPlugin.writeZip(previous, temporary.resolve("previous.zip")), "26.02")
        .setHome(home).setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888).build()
        .apply();
    Path installed = home.resolve("plugins/webforj-bbjship");
    Path plugin = BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship"));
    Path zip = BBjShipPlugin.writeZip(plugin, temporary.resolve("webforj-bbjship.zip"));
    Setup setup = Setup.create(zip, "26.03").setHome(home)
        .setDeploymentUrl(URI.create("http://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setPort(8888)
        .setForce(true).build();

    setup.apply();

    assertEquals("version=26.03\n", Files.readString(installed.resolve("version.properties")));
    assertFalse(Files.exists(installed.resolve("development.enabled")));
    assertFalse(
        Files.exists(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled")));
  }

  private static Stream<Arguments> getInstalledBBjShips() {
    return Stream.of(Arguments.of("26.02", "version=26.02\n"), Arguments.of("26.03", "version=\n"),
        Arguments.of("26.03", "version=26.03\n"));
  }
}
