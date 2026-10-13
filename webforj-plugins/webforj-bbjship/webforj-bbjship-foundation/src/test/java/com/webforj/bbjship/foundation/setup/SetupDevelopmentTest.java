package com.webforj.bbjship.foundation.setup;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.BBjShipPackage;
import com.webforj.bbjship.foundation.ContainerOptionsFile;
import com.webforj.bbjship.foundation.Home;
import com.webforj.bbjship.foundation.ServicesProcess;
import com.webforj.bbjship.foundation.Setup;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.bbjship.foundation.model.JavaSettings;
import com.webforj.bbjship.foundation.setup.VersionServer.RuntimeAnswer;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import org.mockito.MockedConstruction;

class SetupDevelopmentTest {

  private static final String SERVICES_PROPERTIES = """
      basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
      """;
  private static final String SERVICES_DELAY = """
      services
      2
      """;
  private static final String OPTIONS_FOLDER = "options";
  private static final String PLUGIN_NAME = "webforj-bbjship";
  private static final String PLUGIN_ZIP = "webforj-bbjship.zip";
  private static final String SHIP_VERSION = "26.03";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String HOME_MESSAGE = "Using BBj home ";
  private static final String INSTALLING_MESSAGE = "Installing BBjShip 26.03";
  private static final String COMPLETED_MESSAGE = "BBj setup completed";
  private static final String ENABLED_MESSAGE = "BBjShip endpoint is enabled";
  private static final String INVOCATIONS_FILE = "invocations";
  private static final String USERNAME_OPTION = "--username";
  private static final String PASSWORD_STDIN_OPTION = "--password-stdin";
  private static final String REDEFINITION_ARGUMENT = "-XX:+AllowEnhancedClassRedefinition";
  private static final String HOTSWAP_TOOL_ARGUMENT = "-Dwebforj.hotswap.tool=hotswapAgent";
  private static final String PROPERTIES_FILE = "cfg/BBj.properties";
  private static final String UPDATING_MESSAGE = "Updating the Java settings of BBjServices in ";
  private static final String SERVICES_COMMAND = "services";
  private static final String RESTARTING_MESSAGE = "Restarting BBjServices";
  private static final String DELAY_FILE = "sleep";

  @TempDir
  Path temporary;

  @Test
  void shouldSkipRemoteEnableWhenTheEndpointAnswersTheInstalledVersion() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve(OPTIONS_FOLDER);
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setLog(messages::add).build().apply();
    }

    assertEquals(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        COMPLETED_MESSAGE), messages);
    assertFalse(Files.exists(home.resolve(INVOCATIONS_FILE)));
  }

  @Test
  void shouldRunRemoteEnableWhenForceReplacesTheBbjShipOfTheSameVersion() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve(OPTIONS_FOLDER);
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).build().apply();

      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setForce(true).setLog(messages::add).build().apply();
    }

    assertEquals(List.of(HOME_MESSAGE + home.toRealPath(), "Replacing BBjShip 26.03 with 26.03",
        "Enabling BBj remote deployment", COMPLETED_MESSAGE), messages);
    assertEquals(List.of("remote", "enable", USERNAME_OPTION, ADMIN_USERNAME, "--port"),
        Files.readAllLines(home.resolve(INVOCATIONS_FILE)).subList(0, 5));
  }

  @Test
  void shouldEnableDevelopmentRunsWhenTheSwitchIsMissing() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve(OPTIONS_FOLDER);
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true).setLog(messages::add).build()
          .apply();
    }

    assertEquals(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        "Enabling BBj development runs", COMPLETED_MESSAGE), messages);
    assertEquals(List.of("development", "enable", USERNAME_OPTION, ADMIN_USERNAME,
        PASSWORD_STDIN_OPTION, "END"), Files.readAllLines(home.resolve(INVOCATIONS_FILE)));
    assertEquals(List.of(ADMIN_PASSWORD), Files.readAllLines(home.resolve("stdin")));
    assertTrue(
        Files.exists(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled")));
  }

  @Test
  void shouldChangeNothingWhenEverythingIsInPlace() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path java = Path.of(System.getProperty("java.home"), "bin", "java");
    Path file = temporary.resolve("BBj.properties");
    Files.writeString(file, """
        basis.java.jvm.BBjServices=%s
        basis.java.args.BBjServices=-Xmx1024m %s
        """.formatted(java.toAbsolutePath().normalize(),
        ContainerOptionsFile.getArgument(options)));
    final byte[] original = Files.readAllBytes(file);
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true)
          .setJavaSettings(new JavaSettings(file, options)).setLog(messages::add).build();
      setup.apply();
      Files.delete(home.resolve(INVOCATIONS_FILE));
      messages.clear();

      setup.apply();
    }

    assertEquals(
        List.of(HOME_MESSAGE + home.toRealPath(), "BBjShip 26.03 is already installed",
            ENABLED_MESSAGE, "BBjServices runs with the hotswap settings", COMPLETED_MESSAGE),
        messages);
    assertFalse(Files.exists(home.resolve(INVOCATIONS_FILE)));
    assertArrayEquals(original, Files.readAllBytes(file));
  }

  @Test
  void shouldWriteTheJavaSettingsIntoThePropertiesFileOfTheHome() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setLog(messages::add).build().apply();
    }

    Path file = home.toRealPath().resolve(PROPERTIES_FILE);
    Properties written = new Properties();
    try (InputStream input = Files.newInputStream(file)) {
      written.load(input);
    }

    assertNull(written.getProperty("basis.java.jvm.BBjServices"));
    assertEquals("-Xmx1024m -Dfile.encoding=UTF-8 " + ContainerOptionsFile.getArgument(options),
        written.getProperty("basis.java.args.BBjServices"));
    assertEquals(SERVICES_PROPERTIES, Files.readString(
        home.resolve("plugins/.webforj-bbjship-deployments/BBj.properties" + ".before-webforj")));
    assertFalse(Files.exists(home.resolve("cfg/BBj.properties.before-webforj")));
    assertEquals(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        UPDATING_MESSAGE + file, "BBjServices runs with the hotswap settings", COMPLETED_MESSAGE),
        messages);
    assertFalse(Files.exists(home.resolve(INVOCATIONS_FILE)));
  }

  @Test
  void shouldRestartBbjServicesAndWaitUntilTheBbjShipAnswers() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setLog(messages::add).build().apply();

      InOrder order = inOrder(services.constructed().get(0));
      order.verify(services.constructed().get(0)).applyStartValidation();
      order.verify(services.constructed().get(0)).applyRestart(any(BBjShipClient.class));
    }

    assertEquals(List.of(SERVICES_COMMAND, "stop", USERNAME_OPTION, ADMIN_USERNAME,
        PASSWORD_STDIN_OPTION, "END"), Files.readAllLines(home.resolve(INVOCATIONS_FILE)));
    assertEquals(List.of(ADMIN_PASSWORD), Files.readAllLines(home.resolve("stdin")));
    assertLinesMatch(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        UPDATING_MESSAGE + home.toRealPath().resolve(PROPERTIES_FILE), RESTARTING_MESSAGE,
        COMPLETED_MESSAGE), messages);
  }

  @Test
  void shouldFailWhenTheRestartCommandFails() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("failure"), SERVICES_COMMAND);
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBj setup command failed. Check credentials and local BBjServices.",
          failure.getMessage());
      verify(services.constructed().get(0), never()).applyRestart(any(BBjShipClient.class));
    }

    assertFalse(Files.exists(home.resolve("services-restarted")));
  }

  @Test
  void shouldRefuseTheRestartWithoutTheStartProgramBeforeTheCommand() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals(
          "BBjServices start program is missing: " + home.toRealPath().resolve("bin/bbjservices"),
          failure.getMessage());
    }

    assertFalse(Files.exists(home.resolve(INVOCATIONS_FILE)));
  }

  @Test
  void shouldRestartBbjServicesBeyondTheCommandTimeout() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(DELAY_FILE), SERVICES_DELAY);
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true)
          .setJavaSettings(new JavaSettings(null, options)).setCommandTimeout(Duration.ofSeconds(1))
          .setLog(messages::add).build().apply();
    }

    assertLinesMatch(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        "Enabling BBj development runs",
        UPDATING_MESSAGE + home.toRealPath().resolve(PROPERTIES_FILE), RESTARTING_MESSAGE,
        COMPLETED_MESSAGE), messages);
  }

  @Test
  void shouldReportTheWaitForBbjServicesWhileItRestarts() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(DELAY_FILE), SERVICES_DELAY);
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));
    List<String> messages = new ArrayList<>();

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setPollInterval(Duration.ofMillis(100)).setProgressInterval(Duration.ofSeconds(1))
          .setLog(messages::add).build().apply();
    }

    assertLinesMatch(List.of(HOME_MESSAGE + home.toRealPath(), INSTALLING_MESSAGE, ENABLED_MESSAGE,
        UPDATING_MESSAGE + home.toRealPath().resolve(PROPERTIES_FILE), RESTARTING_MESSAGE,
        "Waiting for BBjServices, 1 seconds", "Waiting for BBjServices, 2 seconds",
        COMPLETED_MESSAGE), messages);
  }

  @Test
  void shouldFailWhenTheRestartCommandExceedsTheRestartTimeout() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(DELAY_FILE), SERVICES_DELAY);
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setRestartTimeout(Duration.ofSeconds(1)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBj setup command timed out. Check local BBjServices", failure.getMessage());
    }

    assertFalse(Files.exists(home.resolve("services-restarted")));
  }

  @Test
  void shouldFailWhenBbjServicesRestartsWithoutTheHotswapSettings() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBjServices restarted without the hotswap settings. Check "
          + home.toRealPath().resolve(PROPERTIES_FILE), failure.getMessage());
    }

    assertEquals(List.of(SERVICES_COMMAND, "stop", USERNAME_OPTION, ADMIN_USERNAME,
        PASSWORD_STDIN_OPTION, "END"), Files.readAllLines(home.resolve(INVOCATIONS_FILE)));
  }

  @Test
  void shouldFailOnceWhenBbjShipDoesNotAnswerAfterTheRestartCommandEnded() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve(PROPERTIES_FILE), SERVICES_PROPERTIES);
    Path options = temporary.resolve(OPTIONS_FOLDER);
    ContainerOptionsFile.write(options, List.of(REDEFINITION_ARGUMENT, HOTSWAP_TOOL_ARGUMENT));
    Path zip = BBjShipPackage.writeZip(BBjShipPackage.createCopy(temporary.resolve(PLUGIN_NAME)),
        temporary.resolve(PLUGIN_ZIP));

    try (MockedConstruction<ServicesProcess> services = mockConstruction(ServicesProcess.class);
        VersionServer bbjShip =
            VersionServer.start(home, options, RuntimeAnswer.NO_ANSWER_AFTER_START)) {
      Setup setup = Setup.create(zip, SHIP_VERSION).setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals(
          "BBjShip check could not read the BBjShip version at " + bbjShip.getUrl()
              + ", HTTP 503. Check the deployment URL and run webforj:bbj-setup on the BBj host",
          failure.getMessage());
      assertEquals(1, bbjShip.getRequestsAfterStart());
    }
  }
}
