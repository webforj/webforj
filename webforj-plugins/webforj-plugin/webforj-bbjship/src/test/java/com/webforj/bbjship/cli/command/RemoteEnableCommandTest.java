package com.webforj.bbjship.cli.command;

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
import com.webforj.bbjship.cli.admin.WebServer;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InOrder;
import picocli.CommandLine.Help.Ansi;

class RemoteEnableCommandTest {

  @TempDir
  Path temporary;

  @Test
  void shouldServeTheBbjShipContextAndConfirm() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext("webforj-bbjship", """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "Document Base": "%s"
        }""".formatted(runtime.resolve("htdocs")));
    ByteArrayOutputStream output = new ByteArrayOutputStream();
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run("remote", "enable", "--port", port);
    }

    assertEquals(0, status);
    assertEquals("%s%nEnabled http://localhost:%s/webforj-bbjship%n"
        .formatted(FACTORY_ACCOUNT_WARNING, port), output.toString(StandardCharsets.UTF_8));
    BBjAdminContextInfo bbjShip = services.getContext("webforj-bbjship");
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
    PrintStream stream = new PrintStream(OutputStream.nullOutputStream());
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run("remote", "enable", "--port", port);
    }

    assertEquals(0, status);
    BBjAdminBase admin = services.getAdmin();
    InOrder calls = inOrder(admin, services.getServer());
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", null);
    calls.verify(admin).commit();
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    calls.verify(admin).commit();
    calls.verify(services.getServer()).createContext("webforj-bbjship");
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
    verify(admin, times(2)).setClasspath(anyString(), any());
    verify(services.getContext("webforj-bbjship-assets")).setString(BBjAdminContextInfo.JSON_DATA,
        """
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
            }""".formatted(home.resolve("plugins/webforj-bbjship"), home, port,
            home.resolve("plugins/.webforj-bbjship-deployments")));
  }

  @Test
  void shouldMoveTheAssetsContextToTheNewPortsOfTheBbjShipContext() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    Path plugin = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    BBjServices services = new BBjServices();
    services.addBBjShipConfiguration(home);
    services.addContext("webforj-bbjship", """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(runtime.resolve("htdocs")));
    services.addContext("webforj-bbjship-assets", """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(runtime.resolve("assets")));
    PrintStream stream = new PrintStream(OutputStream.nullOutputStream());
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    String port = Integer.toString(web.getPort());
    String sslPort = Integer.toString(secure.getPort());

    int status;
    try (web; secure) {
      status = cli.run("remote", "enable", "--port", port, "--ssl-port", sslPort);
    }

    assertEquals(0, status);
    BBjAdminContextInfo assets = services.getContext("webforj-bbjship-assets");
    InOrder calls = inOrder(services.getContext("webforj-bbjship"), assets);
    calls.verify(services.getContext("webforj-bbjship")).setString(anyString(), anyString());
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
    PrintStream stream = new PrintStream(OutputStream.nullOutputStream());
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run("remote", "enable", "--port", port);
    }

    assertEquals(0, status);
    List<String> servlet = List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString());
    BBjAdminBase admin = services.getAdmin();
    InOrder calls = inOrder(admin);
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", servlet);
    calls.verify(admin).setClasspath("_webforj_bbjship", List
        .of(home.resolve(".lib/BBj.jar").toString(), home.resolve(".lib/BBjUtil.jar").toString()));
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", null);
    calls.verify(admin).setClasspath("_webforj_bbjship_cli", servlet);
    verify(admin, times(4)).setClasspath(anyString(), any());
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
            }""".formatted(home.resolve("plugins/webforj-bbjship"), home, port,
            home.resolve("plugins/.webforj-bbjship-deployments")));
  }

  @Test
  void shouldStartTheAssetsContextOfAnEmptyServerOnce() throws Exception {
    Path home = Home.createCopy(temporary.resolve("bbx"));
    BBjServices services = new BBjServices();
    PrintStream stream = new PrintStream(OutputStream.nullOutputStream());
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    String port = Integer.toString(web.getPort());

    int status;
    try (web) {
      status = cli.run("remote", "enable", "--port", port);
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
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(
            Map.of("WEBFORJ_BBJ_USERNAME", "administrator", "WEBFORJ_BBJ_PASSWORD", "secret"))
        .setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    String port = Integer.toString(web.getPort());
    String sslPort = Integer.toString(secure.getPort());

    int status;
    try (web; secure) {
      status = cli.run("remote", "enable", "--port", port, "--ssl-port", sslPort);
    }

    assertEquals(0, status);
    assertEquals("""
        Enabled http://localhost:%s/webforj-bbjship
        Enabled https://localhost:%s/webforj-bbjship
        """.formatted(port, sslPort), output.toString(StandardCharsets.UTF_8));
    Path plugin = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(services.getContext("webforj-bbjship")).setString(BBjAdminContextInfo.JSON_DATA, """
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
    PrintStream stream = new PrintStream(output, true, StandardCharsets.UTF_8);
    BBjShipCli cli = BBjShipCli.create(home).setInput(InputStream.nullInputStream())
        .setEnvironment(Map.of()).setOutput(stream).setError(stream).setAnsi(Ansi.OFF)
        .setConnector((username, password) -> services.getAdmin()).build();
    int port = WebServer.findClosedPort();

    int status = cli.run("remote", "enable", "--port", Integer.toString(port));

    assertEquals(1, status);
    assertEquals(
        FACTORY_ACCOUNT_WARNING + "\n"
            + "Command failed. The BBjServices web server does not answer on port %d. "
                .formatted(port)
            + "Use the HTTP port of the BBjServices web server\n",
        output.toString(StandardCharsets.UTF_8));
    verify(services.getServer()).removeContext("webforj-bbjship-assets");
    verify(services.getServer(), never()).createContext("webforj-bbjship");
    assertNull(services.getContext("webforj-bbjship-assets"));
    verify(services.getAdmin()).release();
  }
}
