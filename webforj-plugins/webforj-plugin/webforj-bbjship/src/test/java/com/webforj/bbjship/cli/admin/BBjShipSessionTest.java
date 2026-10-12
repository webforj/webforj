package com.webforj.bbjship.cli.admin;

import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminContextInfo;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.command.BBjServices;
import com.webforj.bbjship.cli.command.Home;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import picocli.CommandLine.Help.Ansi;

class BBjShipSessionTest {

  @TempDir
  Path temporary;

  private Path home;
  private ByteArrayOutputStream output;
  private BBjServices services;
  private BBjShipCli cli;

  @BeforeEach
  void initCliOnAnEmptyServer() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    services = new BBjServices();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream()).setEnvironment(Map.of())
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldAddTheClasspathsAndTheAssetsContextToAnEmptyServer(List<String> command)
      throws Exception {
    int status = cli.run(command.toArray(String[]::new));

    assertEquals(0, status);
    verify(services.getAdmin()).setClasspath("_webforj_bbjship_cli", List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    verify(services.getAdmin()).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    verify(services.getAdmin(), times(2)).setClasspath(anyString(), any());
    verify(services.getServer()).createContext("webforj-bbjship-assets");
    verify(services.getContext("webforj-bbjship-assets")).setString(BBjAdminContextInfo.JSON_DATA,
        """
            {
              "STOP": {
                "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
                "DATA": {
                  "ACTION": "STOP"
                },
                "ENABLED": false
              },
              "START": {
                "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
                "DATA": {
                  "ACTION": "START"
                },
                "ENABLED": true
              },
              "RESTART": {
                "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
                "DATA": {
                  "ACTION": "RESTART"
                },
                "ENABLED": false
              },
              "HTTP port": %3$d,
              "Name": "webforj-bbjship-assets",
              "Path": "/webforj",
              "Document Base": "%4$s/assets",
              "BBx Servlets": [
                {
                  "Enabled": "true",
                  "URL Mapping": "/*",
                  "Source File": "%1$s/servlets/WebforjBBjAssetsServlet.bbj",
                  "Class Name": "WebforjBBjAssetsServlet",
                  "Method Name": "service",
                  "Config File": "%2$s/cfg/config.bbx",
                  "SSCP": "_webforj_bbjship_cli",
                  "Terminal Alias": "",
                  "Working Directory": "",
                  "User": "",
                  "Parameters": []
                }
              ]
            }""".formatted(home.resolve("plugins/webforj-bbjship"), home, 8888,
            home.resolve("plugins/.webforj-bbjship-deployments")));
    verify(services.getAdmin()).release();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldLeaveTheConfigurationOfConfiguredServerAsItIs(List<String> command) throws Exception {
    services.addBBjShipConfiguration(home);

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(0, status);
    verify(services.getAdmin(), never()).setClasspath(anyString(), any());
    verify(services.getServer(), never()).createContext("webforj-bbjship-assets");
    verify(services.getContext("webforj-bbjship-assets"), never()).setString(anyString(),
        anyString());
    verify(services.getAdmin(), never()).jsonExecute(anyString());
    verify(services.getAdmin()).release();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldCreateTheMissingRuntimeFoldersOfConfiguredServer(List<String> command)
      throws Exception {
    services.addBBjShipConfiguration(home);
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(0, status);
    try (Stream<Path> folders = Files.list(runtime)) {
      assertEquals(List.of("assets", "deployments", "htdocs"), folders.filter(Files::isDirectory)
          .map(folder -> folder.getFileName().toString()).sorted().toList());
    }
  }

  @ParameterizedTest
  @MethodSource("getCommands")
  void shouldRefuseTheCommandClasspathOfAnotherApplication(List<String> command) throws Exception {
    services.addClasspath("_webforj_bbjship_cli", List.of("(bbj_default)", "/srv/other.jar"));

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. The BBjShip command classpath belongs to another application\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getAdmin(), never()).setClasspath(anyString(), any());
    verify(services.getServer(), never()).createContext(anyString());
    verify(services.getAdmin()).release();
    assertFalse(Files.exists(home.resolve("plugins/.webforj-bbjship-deployments")));
  }

  @ParameterizedTest
  @MethodSource("getCommands")
  void shouldRefusePluginFolderThatIsSymbolicLinkBeforeOpeningTheSession(List<String> command)
      throws Exception {
    Path moved = Files.move(home.resolve("plugins/webforj-bbjship"), temporary.resolve("moved"));
    Files.createSymbolicLink(home.resolve("plugins/webforj-bbjship"), moved);

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(1, status);
    assertEquals(
        "Command failed. The plugin directory is a symbolic link. "
            + "Install the plugin into a regular directory\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getAdmin(), never()).getClasspathNames();
  }

  private static Stream<Arguments> getCommands() {
    return Stream.concat(Stream.of(Arguments.of(List.of("remote", "enable", "--port", "8889"))),
        getCommandsKeepingTheConfiguration());
  }

  private static Stream<Arguments> getCommandsKeepingTheConfiguration() {
    return Stream.of(Arguments.of(List.of("remote", "disable")),
        Arguments.of(List.of("apps", "list")), Arguments.of(List.of("apps", "undeploy", "--all")));
  }
}
