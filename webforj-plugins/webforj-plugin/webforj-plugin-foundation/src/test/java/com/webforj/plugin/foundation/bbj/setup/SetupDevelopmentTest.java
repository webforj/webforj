package com.webforj.plugin.foundation.bbj.setup;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertLinesMatch;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import com.webforj.plugin.foundation.bbj.Home;
import com.webforj.plugin.foundation.bbj.Setup;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.JavaSettings;
import com.webforj.plugin.foundation.bbj.setup.VersionServer.RuntimeAnswer;
import com.webforj.plugin.foundation.hotswap.ContainerOptionsFile;
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

class SetupDevelopmentTest {

  @TempDir
  Path temporary;

  @Test
  void shouldSkipRemoteEnableWhenTheEndpointAnswersTheInstalledVersion() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve("options");
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setLog(messages::add).build().apply();
    }

    assertEquals(List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
        "BBjShip endpoint is enabled", "BBj setup completed"), messages);
    assertFalse(Files.exists(home.resolve("invocations")));
  }

  @Test
  void shouldEnableDevelopmentRunsWhenTheSwitchIsMissing() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve("options");
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true).setLog(messages::add).build()
          .apply();
    }

    assertEquals(
        List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
            "BBjShip endpoint is enabled", "Enabling BBj development runs", "BBj setup completed"),
        messages);
    assertEquals(
        List.of("development", "enable", "--username", "administrator", "--password-stdin", "END"),
        Files.readAllLines(home.resolve("invocations")));
    assertEquals(List.of("secret-password"), Files.readAllLines(home.resolve("stdin")));
    assertTrue(
        Files.exists(home.resolve("plugins/.webforj-bbjship-deployments/development.enabled")));
  }

  @Test
  void shouldChangeNothingWhenEverythingIsInPlace() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path java = Path.of(System.getProperty("java.home"), "bin", "java");
    Path file = temporary.resolve("BBj.properties");
    Files.writeString(file, """
        basis.java.jvm.BBjServices=%s
        basis.java.args.BBjServices=-Xmx1024m %s
        """.formatted(java.toAbsolutePath().normalize(),
        ContainerOptionsFile.getArgument(options)));
    final byte[] original = Files.readAllBytes(file);
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS)) {
      Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true)
          .setJavaSettings(new JavaSettings(file, options)).setLog(messages::add).build();
      setup.apply();
      Files.delete(home.resolve("invocations"));
      messages.clear();

      setup.apply();
    }

    assertEquals(List.of("Using BBj home " + home.toRealPath(),
        "BBjShip 26.03 is already installed", "BBjShip endpoint is enabled",
        "BBjServices runs with the hotswap settings", "BBj setup completed"), messages);
    assertFalse(Files.exists(home.resolve("invocations")));
    assertArrayEquals(original, Files.readAllBytes(file));
  }

  @Test
  void shouldWriteTheJavaSettingsIntoThePropertiesFileOfTheHome() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setLog(messages::add).build().apply();
    }

    Path file = home.toRealPath().resolve("cfg/BBj.properties");
    Properties written = new Properties();
    try (InputStream input = Files.newInputStream(file)) {
      written.load(input);
    }

    assertNull(written.getProperty("basis.java.jvm.BBjServices"));
    assertEquals("-Xmx1024m -Dfile.encoding=UTF-8 " + ContainerOptionsFile.getArgument(options),
        written.getProperty("basis.java.args.BBjServices"));
    assertEquals("basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8\n", Files.readString(
        home.resolve("plugins/.webforj-bbjship-deployments/BBj.properties" + ".before-webforj")));
    assertFalse(Files.exists(home.resolve("cfg/BBj.properties.before-webforj")));
    assertEquals(List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
        "BBjShip endpoint is enabled", "Updating the Java settings of BBjServices in " + file,
        "BBjServices runs with the hotswap settings", "BBj setup completed"), messages);
    assertFalse(Files.exists(home.resolve("invocations")));
  }

  @Test
  void shouldRestartBbjServicesAndWaitUntilTheBbjShipAnswers() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setLog(messages::add).build().apply();
    }

    assertEquals(
        List.of("services", "restart", "--username", "administrator", "--password-stdin", "END"),
        Files.readAllLines(home.resolve("invocations")));
    assertEquals(List.of("secret-password"), Files.readAllLines(home.resolve("stdin")));
    assertLinesMatch(List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
        "BBjShip endpoint is enabled",
        "Updating the Java settings of BBjServices in "
            + home.toRealPath().resolve("cfg/BBj.properties"),
        "Restarting BBjServices", "BBjServices answered after \\d+ seconds", "BBj setup completed"),
        messages);
  }

  @Test
  void shouldFailWhenTheRestartCommandFails() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("failure"), "services");
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBj setup command failed. Check credentials and local BBjServices.",
          failure.getMessage());
    }

    assertFalse(Files.exists(home.resolve("services-restarted")));
  }

  @Test
  void shouldRestartBbjServicesBeyondTheCommandTimeout() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("sleep"), """
        services
        2
        """);
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setDevelopmentEnabled(true)
          .setJavaSettings(new JavaSettings(null, options)).setCommandTimeout(Duration.ofSeconds(1))
          .setLog(messages::add).build().apply();
    }

    assertLinesMatch(List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
        "BBjShip endpoint is enabled", "Enabling BBj development runs",
        "Updating the Java settings of BBjServices in "
            + home.toRealPath().resolve("cfg/BBj.properties"),
        "Restarting BBjServices", "BBjServices answered after \\d+ seconds", "BBj setup completed"),
        messages);
  }

  @Test
  void shouldReportTheWaitForBbjServicesWhileItRestarts() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("sleep"), """
        services
        2
        """);
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));
    List<String> messages = new ArrayList<>();

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setPollInterval(Duration.ofMillis(100)).setProgressInterval(Duration.ofSeconds(1))
          .setLog(messages::add).build().apply();
    }

    assertLinesMatch(List.of("Using BBj home " + home.toRealPath(), "Installing BBjShip 26.03",
        "BBjShip endpoint is enabled",
        "Updating the Java settings of BBjServices in "
            + home.toRealPath().resolve("cfg/BBj.properties"),
        "Restarting BBjServices", "Waiting for BBjServices, 1 seconds",
        "Waiting for BBjServices, 2 seconds", "BBjServices answered after \\d+ seconds",
        "BBj setup completed"), messages);
  }

  @Test
  void shouldFailWhenTheRestartCommandExceedsTheRestartTimeout() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("sleep"), """
        services
        2
        """);
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));

    try (VersionServer bbjShip =
        VersionServer.start(home, options, RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START)) {
      Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
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
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBjServices restarted without the hotswap settings. Check "
          + home.toRealPath().resolve("cfg/BBj.properties"), failure.getMessage());
    }

    assertEquals(
        List.of("services", "restart", "--username", "administrator", "--password-stdin", "END"),
        Files.readAllLines(home.resolve("invocations")));
  }

  @Test
  void shouldFailWhenTheBbjShipDoesNotAnswerWithinTheStartTimeout() throws IOException {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Files.writeString(home.resolve("cfg/BBj.properties"), """
        basis.java.args.BBjServices=-Xmx1024m -Dfile.encoding=UTF-8
        """);
    Path options = temporary.resolve("options");
    ContainerOptionsFile.write(options,
        List.of("-XX:+AllowEnhancedClassRedefinition", "-Dwebforj.hotswap.tool=hotswapAgent"));
    Path zip =
        BBjShipPlugin.writeZip(BBjShipPlugin.createCopy(temporary.resolve("webforj-bbjship")),
            temporary.resolve("webforj-bbjship.zip"));

    try (VersionServer bbjShip = VersionServer.start(home, options, RuntimeAnswer.OTHER_SETTINGS)) {
      Setup setup = Setup.create(zip, "26.03").setHome(home).setDeploymentUrl(bbjShip.getUrl())
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setPort(bbjShip.getPort()).setJavaSettings(new JavaSettings(null, options))
          .setStartTimeout(Duration.ZERO).setPollInterval(Duration.ofSeconds(2)).build();

      IOException failure = assertThrows(IOException.class, setup::apply);

      assertEquals("BBjServices did not answer at " + bbjShip.getUrl()
          + " within 0 seconds after the restart", failure.getMessage());
    }
  }
}
