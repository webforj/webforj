package com.webforj.plugin.foundation.bbj.setup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.BBjShipPackage;
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

  private static final String PLUGIN_NAME = "webforj-bbjship";
  private static final String PLUGIN_ZIP = "webforj-bbjship.zip";
  private static final String SHIP_VERSION = "26.03";
  private static final String DEPLOYMENT_URL = "http://localhost:1/webforj-bbjship";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String PLUGIN_PATH = "plugins/webforj-bbjship";
  private static final String VERSION_FILE = "version.properties";
  private static final String PLUGIN_DESCRIPTOR = "bbj-plugin.json";
  private static final String PLUGIN_SCRIPT = "bbjship";
  private static final String INVOCATIONS_FILE = "invocations";
  private static final String PREVIOUS_PLUGIN_NAME = "webforj-bbjship-26.02";
  private static final String PREVIOUS_VERSION_LINE = "version=26.02\n";
  private static final String PREVIOUS_ZIP = "previous.zip";
  private static final String PREVIOUS_VERSION = "26.02";
  private static final String INSTALLED_VERSION_FILE = "plugins/webforj-bbjship/version.properties";
  private static final String APP_DEPLOYMENT = "deployments/hello-app";
  private static final String APP_JAR = "deployments/hello-app/application.jar";
  private static final String APP_JAR_CONTENT = "hello-app";
  private static final String VERSION_LINE = "version=26.03\n";
  private static final String DEVELOPMENT_FLAG = "development.enabled";

  @TempDir
  Path temporary;

  @Test
  void shouldInstallBbjShipAndRunRemoteEnableWithPasswordOnStandardInput() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setLog(messages::add).build();

    setup.apply();

    Path installed = home.resolve(PLUGIN_PATH);
    try (Stream<Path> files = Files.walk(installed)) {
      assertEquals(
          List.of(PLUGIN_DESCRIPTOR, PLUGIN_SCRIPT, "lib/webforj-bbjship.jar", VERSION_FILE),
          files.filter(Files::isRegularFile).map(file -> installed.relativize(file).toString())
              .sorted().toList());
    }

    assertTrue(Files.isExecutable(installed.resolve(PLUGIN_SCRIPT)));
    assertEquals("""
        remote
        enable
        --username
        administrator
        --port
        8888
        --password-stdin
        END
        """, Files.readString(home.resolve(INVOCATIONS_FILE)));
    assertEquals("""
        secret-password
        """, Files.readString(home.resolve("stdin")));
    assertEquals("", Files.readString(home.resolve("workspace-properties")));
    assertTrue(Files.exists(home.resolve("remote-enabled")));
    assertEquals(List.of("Using BBj home " + home, "Installing BBjShip 26.03",
        "Enabling BBj remote deployment", "BBj setup completed"), messages);
  }

  @Test
  void shouldRunBbjShipCommandsWithoutColors() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setLog(message -> {
            }).build();

    setup.apply();

    assertEquals("1\n", Files.readString(home.resolve("no-color")));
  }

  @Test
  void shouldRunRemoteEnableOnTheSslPortOfHttpsDeploymentUrl() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home)
        .setDeploymentUrl(URI.create("https://localhost:1/webforj-bbjship"))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8443)
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
        """, Files.readString(home.resolve(INVOCATIONS_FILE)));
  }

  @Test
  void shouldRemoveFreshBbjShipWhenRemoteEnableFails() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx")).toRealPath();
    Files.writeString(home.resolve("failure"), "remote");
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
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
        """, Files.readString(home.resolve(INVOCATIONS_FILE)));
    assertEquals("""
        secret-password
        """, Files.readString(home.resolve("stdin")));
    assertFalse(Files.exists(home.resolve("remote-enabled")));
    assertFalse(Files.exists(home.resolve(PLUGIN_PATH)));
  }

  @Test
  void shouldRefuseInstalledBbjShipOfAnotherVersionWithoutForce() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPackage.createCopy(temporary.resolve(PREVIOUS_PLUGIN_NAME));
    Files.writeString(previous.resolve(VERSION_FILE), PREVIOUS_VERSION_LINE);
    Setup
        .create(BBjShipPackage.writeZip(previous, temporary.resolve(PREVIOUS_ZIP)),
            PREVIOUS_VERSION)
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    String invocations = Files.readString(home.resolve(INVOCATIONS_FILE));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .build();

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(
        "Installed BBjShip version 26.02 does not match 26.03. "
            + "Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace it",
        failure.getMessage());
    assertEquals(PREVIOUS_VERSION_LINE, Files.readString(home.resolve(INSTALLED_VERSION_FILE)));
    assertEquals(invocations, Files.readString(home.resolve(INVOCATIONS_FILE)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"version=\n", ""})
  void shouldRefuseInstalledBbjShipWithoutVersionWithoutForce(String installedVersion)
      throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .build();
    setup.apply();
    Files.writeString(home.resolve(INSTALLED_VERSION_FILE), installedVersion);
    String invocations = Files.readString(home.resolve(INVOCATIONS_FILE));

    IOException failure = assertThrows(IOException.class, setup::apply);

    assertEquals(
        "Installed BBjShip version is missing. "
            + "Run webforj:bbj-setup with -Dwebforj.bbj.force=true to replace the package",
        failure.getMessage());
    assertEquals(installedVersion, Files.readString(home.resolve(INSTALLED_VERSION_FILE)));
    assertEquals(invocations, Files.readString(home.resolve(INVOCATIONS_FILE)));
  }

  @ParameterizedTest
  @MethodSource("getInstalledBBjShips")
  void shouldReplaceInstalledBbjShipWithForceAndLeaveTheRuntimeFolderUntouched(String version,
      String installedVersion) throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPackage.createCopy(temporary.resolve("webforj-bbjship-" + version));
    Files.writeString(previous.resolve(VERSION_FILE), "version=" + version + "\n");
    Setup.create(BBjShipPackage.writeZip(previous, temporary.resolve(PREVIOUS_ZIP)), version)
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    Path installed = home.resolve(PLUGIN_PATH);
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    Files.writeString(installed.resolve(VERSION_FILE), installedVersion);
    Files.writeString(installed.resolve(PLUGIN_DESCRIPTOR), "{}");
    Files.createDirectories(installed.resolve(APP_DEPLOYMENT));
    Files.writeString(installed.resolve(APP_JAR), APP_JAR_CONTENT);
    Files.createDirectories(installed.resolve("htdocs/hello-app"));
    Files.writeString(installed.resolve("htdocs/hello-app/index.html"), "<html></html>");
    Files.createDirectories(installed.resolve("assets/hello-app"));
    Files.writeString(installed.resolve("assets/hello-app/app.css"), ":root {}");
    Files.createDirectories(installed.resolve("commands"));
    Files.writeString(installed.resolve("commands/Removed.bbj"), "rem removed");
    Files.writeString(installed.resolve("lib/removed.jar"), "removed");
    Files.createDirectories(installed.resolve("configuration"));
    Files.writeString(installed.resolve("configuration/local.properties"), "removed=true");
    Files.createDirectories(runtime.resolve(APP_DEPLOYMENT));
    Files.writeString(runtime.resolve(APP_JAR), APP_JAR_CONTENT);
    Files.createDirectories(runtime.resolve("htdocs"));
    Files.createDirectories(runtime.resolve("assets"));
    Files.createDirectories(home.resolve("plugins/other"));
    Files.writeString(home.resolve("plugins/other/plugin.bbj"), "rem other plugin");
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setForce(true).build();

    setup.apply();

    assertEquals(VERSION_LINE, Files.readString(installed.resolve(VERSION_FILE)));
    assertEquals("", Files.readString(installed.resolve(PLUGIN_DESCRIPTOR)));
    try (Stream<Path> files = Files.walk(installed)) {
      assertEquals(
          List.of(PLUGIN_DESCRIPTOR, PLUGIN_SCRIPT, "lib/webforj-bbjship.jar", VERSION_FILE),
          files.filter(Files::isRegularFile).map(file -> installed.relativize(file).toString())
              .sorted().toList());
    }

    try (Stream<Path> files = Files.walk(runtime)) {
      assertEquals(List.of("", "assets", "deployments", APP_DEPLOYMENT, APP_JAR, "htdocs"),
          files.map(file -> runtime.relativize(file).toString()).sorted().toList());
    }

    assertEquals(APP_JAR_CONTENT, Files.readString(runtime.resolve(APP_JAR)));
    assertEquals("rem other plugin", Files.readString(home.resolve("plugins/other/plugin.bbj")));
  }

  @Test
  void shouldKeepDevelopmentSwitchWhenForceReplacesInstalledBbjShip() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPackage.createCopy(temporary.resolve(PREVIOUS_PLUGIN_NAME));
    Files.writeString(previous.resolve(VERSION_FILE), PREVIOUS_VERSION_LINE);
    Setup
        .create(BBjShipPackage.writeZip(previous, temporary.resolve(PREVIOUS_ZIP)),
            PREVIOUS_VERSION)
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    Path installed = home.resolve(PLUGIN_PATH);
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    Files.writeString(runtime.resolve(DEVELOPMENT_FLAG),
        "development runs are enabled on this machine");
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setForce(true).build();

    setup.apply();

    assertEquals(VERSION_LINE, Files.readString(installed.resolve(VERSION_FILE)));
    assertEquals("development runs are enabled on this machine",
        Files.readString(runtime.resolve(DEVELOPMENT_FLAG)));
    assertFalse(Files.exists(installed.resolve(DEVELOPMENT_FLAG)));
  }

  @Test
  void shouldNotCreateDevelopmentSwitchWhenForceReplacesBbjShipWithoutOne() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path previous = BBjShipPackage.createCopy(temporary.resolve(PREVIOUS_PLUGIN_NAME));
    Files.writeString(previous.resolve(VERSION_FILE), PREVIOUS_VERSION_LINE);
    Setup
        .create(BBjShipPackage.writeZip(previous, temporary.resolve(PREVIOUS_ZIP)),
            PREVIOUS_VERSION)
        .setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
        .build().apply();
    Path installed = home.resolve(PLUGIN_PATH);
    Path plugin = BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME));
    Path zip = BBjShipPackage.writeZip(plugin, temporary.resolve(PLUGIN_ZIP));
    Setup setup =
        Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(URI.create(DEPLOYMENT_URL))
            .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).setPort(8888)
            .setForce(true).build();

    setup.apply();

    assertEquals(VERSION_LINE, Files.readString(installed.resolve(VERSION_FILE)));
    assertFalse(Files.exists(installed.resolve(DEVELOPMENT_FLAG)));
    assertFalse(
        Files.exists(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled")));
  }

  private static Stream<Arguments> getInstalledBBjShips() {
    return Stream.of(Arguments.of(PREVIOUS_VERSION, PREVIOUS_VERSION_LINE),
        Arguments.of(SHIP_VERSION, "version=\n"), Arguments.of(SHIP_VERSION, VERSION_LINE));
  }
}
