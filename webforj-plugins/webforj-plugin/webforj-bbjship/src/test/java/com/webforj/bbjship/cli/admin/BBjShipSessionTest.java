package com.webforj.bbjship.cli.admin;

import static com.webforj.bbjship.cli.command.BBjServices.ASSETS_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.CLI_CLASSPATH;
import static com.webforj.bbjship.cli.command.Home.DEPLOYMENTS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.PLUGIN_FOLDER;
import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminException;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.TerminalLines;
import com.webforj.bbjship.cli.command.BBjServices;
import com.webforj.bbjship.cli.command.Home;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
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
  void initCliOnAnEmptyServer() throws IOException, URISyntaxException, BBjAdminException {
    home = Home.createCopy(temporary.resolve("bbx"));
    output = new ByteArrayOutputStream();
    services = new BBjServices();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    cli = BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
        .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldAddTheClasspathsAndTheAssetsContextToAnEmptyServer(List<String> command)
      throws Exception {
    int status = cli.run(command.toArray(String[]::new));

    assertEquals(0, status);
    verify(services.getAdmin()).setClasspath(CLI_CLASSPATH, List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    verify(services.getAdmin()).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    verify(services.getAdmin(), times(2)).setClasspath(anyString(), any());
    verify(services.getServer()).createContext(ASSETS_CONTEXT);
    verify(services.getContext(ASSETS_CONTEXT)).setString(BBjAdminContextInfo.JSON_DATA, """
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
        }""".formatted(home.resolve(PLUGIN_FOLDER), home, 8888, home.resolve(DEPLOYMENTS_FOLDER)));
    verify(services.getAdmin()).release();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldLeaveTheConfigurationOfConfiguredServerAsItIs(List<String> command) throws Exception {
    services.addBBjShipConfiguration(home);

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(0, status);
    verify(services.getAdmin(), never()).setClasspath(anyString(), any());
    verify(services.getServer(), never()).createContext(ASSETS_CONTEXT);
    verify(services.getContext(ASSETS_CONTEXT), never()).setString(anyString(), anyString());
    verify(services.getAdmin(), never()).jsonExecute(anyString());
    verify(services.getAdmin()).release();
  }

  @ParameterizedTest
  @MethodSource("getCommandsKeepingTheConfiguration")
  void shouldCreateTheMissingRuntimeFoldersOfConfiguredServer(List<String> command)
      throws Exception {
    services.addBBjShipConfiguration(home);
    Path runtime = home.resolve(DEPLOYMENTS_FOLDER);

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
    services.addClasspath(CLI_CLASSPATH, List.of("(bbj_default)", "/srv/other.jar"));

    int status = cli.run(command.toArray(String[]::new));

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. The BBjShip command classpath belongs to another application\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getAdmin(), never()).setClasspath(anyString(), any());
    verify(services.getServer(), never()).createContext(anyString());
    verify(services.getAdmin()).release();
    assertFalse(Files.exists(home.resolve(DEPLOYMENTS_FOLDER)));
  }

  @ParameterizedTest
  @MethodSource("getCommands")
  void shouldRefusePluginFolderThatIsSymbolicLinkBeforeOpeningTheSession(List<String> command)
      throws Exception {
    Path moved = Files.move(home.resolve(PLUGIN_FOLDER), temporary.resolve("moved"));
    Files.createSymbolicLink(home.resolve(PLUGIN_FOLDER), moved);

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
