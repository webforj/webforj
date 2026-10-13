package com.webforj.bbjship.cli.command;

import static com.webforj.bbjship.cli.command.BBjServices.ASSETS_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.BBJSHIP_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.CLI_CLASSPATH;
import static com.webforj.bbjship.cli.command.Home.DEPLOYMENTS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.PLUGIN_FOLDER;
import static com.webforj.plugin.foundation.bbj.model.BBjCredentials.FACTORY_ACCOUNT_WARNING;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminContextInfo;
import com.webforj.bbjship.cli.BBjShipCli;
import com.webforj.bbjship.cli.TerminalLines;
import com.webforj.bbjship.cli.admin.WebServer;
import com.webforj.plugin.foundation.bbj.BBjShipPlugin;
import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import picocli.CommandLine.Help.Ansi;

class RemoteEnableCommandTest {

  private static final String PORT_OPTION = "--port";
  private static final String REMOTE_COMMAND = "remote";
  private static final String ENABLE_COMMAND = "enable";

  @TempDir
  Path temporary;

  @Test
  void shouldServeTheBbjShipContextAndConfirm() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    final Path plugin = home.resolve(PLUGIN_FOLDER);
    Path runtime = home.resolve(DEPLOYMENTS_FOLDER);
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext(BBJSHIP_CONTEXT, """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "Document Base": "%s"
        }""".formatted(runtime.resolve("htdocs")));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port);
    }

    assertEquals(0, status);
    assertEquals("%s%nEnabled http://localhost:%s/webforj-bbjship%n"
        .formatted(FACTORY_ACCOUNT_WARNING, port), output.toString(StandardCharsets.UTF_8));
    BBjAdminContextInfo bbjShip = services.getContext(BBJSHIP_CONTEXT);
    verify(bbjShip).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": true
          },
          "Document Base": "%4$s/htdocs",
          "Path": "/webforj-bbjship",
          "HTTP port": %3$s,
          "Protocol": true,
          "SSL port": "",
          "BBx Servlets": [
            {
              "Enabled": "true",
              "URL Mapping": "/*",
              "Source File": "%1$s/servlets/WebforjBBjDeploymentServlet.bbj",
              "Class Name": "WebforjBBjDeploymentServlet",
              "Method Name": "service",
              "Config File": "%2$s/cfg/config.bbx",
              "SSCP": "_webforj_bbjship_cli",
              "Terminal Alias": "",
              "Working Directory": "",
              "User": "",
              "Parameters": []
            }
          ]
        }""".formatted(plugin, home, port, runtime));
    verify(services.getAdmin()).release();
  }

  @Test
  void shouldResetTheServletClasspathAndRestartTheAssetsContextOfConfiguredServer()
      throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    Writer stream = Writer.nullWriter();
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port);
    }

    assertEquals(0, status);
    BBjAdminBase admin = services.getAdmin();
    InOrder calls = inOrder(admin, services.getServer());
    calls.verify(admin).setClasspath(CLI_CLASSPATH, null);
    calls.verify(admin).commit();
    calls.verify(admin).setClasspath(CLI_CLASSPATH, List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    calls.verify(admin).commit();
    calls.verify(services.getServer()).createContext(BBJSHIP_CONTEXT);
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
    verify(admin, times(2)).setClasspath(anyString(), any());
    verify(services.getContext(ASSETS_CONTEXT)).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "START": {
            "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
            "DATA": {
              "ACTION": "START"
            },
            "ENABLED": false
          },
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": true
          },
          "Document Base": "%4$s/assets",
          "Path": "/webforj",
          "HTTP port": %3$s,
          "Protocol": true,
          "SSL port": "",
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
        }""".formatted(home.resolve(PLUGIN_FOLDER), home, port, home.resolve(DEPLOYMENTS_FOLDER)));
  }

  @Test
  void shouldMoveTheAssetsContextToTheNewPortsOfTheBbjShipContext() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    final Path plugin = home.resolve(PLUGIN_FOLDER);
    Path runtime = home.resolve(DEPLOYMENTS_FOLDER);
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext(BBJSHIP_CONTEXT, """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(runtime.resolve("htdocs")));
    services.addContext(ASSETS_CONTEXT, """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(runtime.resolve("assets")));
    Writer stream = Writer.nullWriter();
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    String port = Integer.toString(web.getPort());
    String sslPort = Integer.toString(secure.getPort());

    int status;
    try (web; secure) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port, "--ssl-port", sslPort);
    }

    assertEquals(0, status);
    BBjAdminContextInfo assets = services.getContext(ASSETS_CONTEXT);
    InOrder calls = inOrder(services.getContext(BBJSHIP_CONTEXT), assets);
    calls.verify(services.getContext(BBJSHIP_CONTEXT)).setString(anyString(), anyString());
    calls.verify(assets).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": true
          },
          "HTTP port": %3$s,
          "Protocol": true,
          "SSL port": %4$s,
          "Document Base": "%5$s/assets",
          "Path": "/webforj",
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
        }""".formatted(plugin, home, port, sslPort, runtime));
  }

  @Test
  void shouldConfigureAnEmptyServerOnThePortOfTheCommand() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    Writer stream = Writer.nullWriter();
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port);
    }

    assertEquals(0, status);
    List<String> servlet = List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString());
    BBjAdminBase admin = services.getAdmin();
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath(CLI_CLASSPATH, servlet);
    calls.verify(admin).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    calls.verify(admin).setClasspath(CLI_CLASSPATH, null);
    calls.verify(admin).setClasspath(CLI_CLASSPATH, servlet);
    verify(admin, times(4)).setClasspath(anyString(), any());
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
          "HTTP port": %3$s,
          "Name": "webforj-bbjship-assets",
          "Path": "/webforj",
          "Document Base": "%4$s/assets",
          "Protocol": true,
          "SSL port": "",
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
        }""".formatted(home.resolve(PLUGIN_FOLDER), home, port, home.resolve(DEPLOYMENTS_FOLDER)));
  }

  @Test
  void shouldStartTheAssetsContextOfAnEmptyServerOnce() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    Writer stream = Writer.nullWriter();
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port);
    }

    assertEquals(0, status);
    verify(services.getAdmin(), times(1)).jsonExecute(contains("CONTEXT:webforj-bbjship-assets"));
  }

  @Test
  void shouldServeTheBbjShipContextOnTheHttpAndTheSslPortAndConfirm() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(new BBjShipPlugin(home))
        .setTerminal(new TerminalLines(stream).setVariables(
            Map.of("WEBFORJ_BBJ_USERNAME", "administrator", "WEBFORJ_BBJ_PASSWORD", "secret")))
        .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    String port = Integer.toString(web.getPort());
    String sslPort = Integer.toString(secure.getPort());

    int status;
    try (web; secure) {
      status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, port, "--ssl-port", sslPort);
    }

    assertEquals(0, status);
    assertEquals("""
        Enabled http://localhost:%s/webforj-bbjship
        Enabled https://localhost:%s/webforj-bbjship
        """.formatted(port, sslPort), output.toString(StandardCharsets.UTF_8));
    Path plugin = home.resolve(PLUGIN_FOLDER);
    Path runtime = home.resolve(DEPLOYMENTS_FOLDER);
    verify(services.getContext(BBJSHIP_CONTEXT)).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "STOP": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "STOP"
            },
            "ENABLED": false
          },
          "START": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "START"
            },
            "ENABLED": true
          },
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": false
          },
          "HTTP port": %3$s,
          "Name": "webforj-bbjship",
          "Path": "/webforj-bbjship",
          "Document Base": "%5$s/htdocs",
          "Protocol": true,
          "SSL port": %4$s,
          "BBx Servlets": [
            {
              "Enabled": "true",
              "URL Mapping": "/*",
              "Source File": "%1$s/servlets/WebforjBBjDeploymentServlet.bbj",
              "Class Name": "WebforjBBjDeploymentServlet",
              "Method Name": "service",
              "Config File": "%2$s/cfg/config.bbx",
              "SSCP": "_webforj_bbjship_cli",
              "Terminal Alias": "",
              "Working Directory": "",
              "User": "",
              "Parameters": []
            }
          ]
        }""".formatted(plugin, home, port, sslPort, runtime));
    verify(services.getServer(), never()).removeContext(anyString());
  }

  @Test
  void shouldRefusePortWithoutTheWebServerAndRemoveTheAssetsContext() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    Writer stream = new OutputStreamWriter(output, StandardCharsets.UTF_8);
    BBjShipCli cli =
        BBjShipCli.create(new BBjShipPlugin(home)).setTerminal(new TerminalLines(stream))
            .setAnsi(Ansi.OFF).setSessions(login -> services.getAdmin())
            .setAnswerDeadline(Duration.ofMillis(300)).build();
    int port = WebServer.findClosedPort();

    int status = cli.run(REMOTE_COMMAND, ENABLE_COMMAND, PORT_OPTION, Integer.toString(port));

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. The BBjServices web server does not answer on port %d. "
                .formatted(port)
            + "Use the HTTP port of the BBjServices web server\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getServer()).removeContext(ASSETS_CONTEXT);
    verify(services.getServer(), never()).createContext(BBJSHIP_CONTEXT);
    assertNull(services.getContext(ASSETS_CONTEXT));
    verify(services.getAdmin()).release();
  }
}
