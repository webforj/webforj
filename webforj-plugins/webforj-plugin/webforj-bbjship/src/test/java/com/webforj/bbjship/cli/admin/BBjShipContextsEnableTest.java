package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminWebAppServer;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.OptionalInt;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InOrder;

class BBjShipContextsEnableTest extends EmptyWebAppServer {

  @Test
  void shouldCreateTheBbjShipContextAndStartIt() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();

    try (web) {
      contexts.enable(port, OptionalInt.empty(), false);
    }

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    InOrder calls = inOrder(created, server, admin);
    calls.verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
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
          "Auto Start": true,
          "HTTP port": %3$d,
          "Name": "webforj-bbjship",
          "Path": "/webforj-bbjship",
          "Document Base": "%4$s/htdocs",
          "SSCP": "",
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
          ],
          "Welcome Files": [
            "index.html",
            "index.htm",
            "index.bbjsp"
          ],
          "Protocol": true,
          "SSL port": ""
        }""".formatted(folder, home, port, runtime));
    calls.verify(created).commit();
    calls.verify(server).commit();
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "START"
          },
          "ENABLED": true
        }""");
    assertTrue(Files.isDirectory(runtime.resolve("htdocs")));
  }

  @Test
  void shouldUpdateTheRunningBbjShipContextAndRestartIt() throws Exception {
    Path documentRoot = home.resolve("plugins/.webforj-bbjship-deployments/htdocs");
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted("webforj-bbjship", documentRoot));
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();

    try (web) {
      contexts.enable(port, OptionalInt.empty(), false);
    }

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(server, never()).createContext(anyString());
    verify(info).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "STOP": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "STOP"
            },
            "ENABLED": true
          },
          "START": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "START"
            },
            "ENABLED": false
          },
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": true
          },
          "Name": "webforj-bbjship",
          "Document Base": "%4$s/htdocs",
          "Path": "/webforj-bbjship",
          "HTTP port": %3$d,
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
        }""".formatted(folder, home, port, runtime));
    verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
  }

  @ParameterizedTest
  @MethodSource("getBBjShipChallenges")
  void shouldAcceptTheBbjShipChallenge(String challenge) throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    WebServer web = WebServer.startServingBBjShipWith(challenge, 0);
    int httpPort = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    try (web) {
      assertDoesNotThrow(() -> contexts.enable(httpPort, sslPort, false));
    }

    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldWaitForTheBbjShipContextThatAnswersNotFoundWhileItStarts() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    WebServer web = WebServer.startServingBBjShipWith(
        "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"", 2);
    int httpPort = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    try (web) {
      assertDoesNotThrow(() -> contexts.enable(httpPort, sslPort, false));
    }

    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldWaitForTheBbjShipContextWhosePortRefusesConnectionsWhileItStarts() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    WebServer web = WebServer.startServingBBjShipAfterClosing(3);
    int httpPort = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    try (web) {
      assertDoesNotThrow(() -> contexts.enable(httpPort, sslPort, false));
    }

    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldRestoreTheBbjShipContextWhenTheBbjShipDoesNotAnswerOnThePort() throws Exception {
    Path documentRoot = home.resolve("plugins/.webforj-bbjship-deployments/htdocs");
    String running = RUNNING.formatted("webforj-bbjship", documentRoot);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    contexts = new BBjShipContexts(admin, plugin, Duration.ofMillis(300));
    WebServer web = WebServer.startServingOther();
    int port = web.getPort();
    int httpPort = port;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));
    }

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the HTTP port of the BBjServices web server", failure.getMessage());
    InOrder calls = inOrder(info, server, admin);
    calls.verify(info).setString(anyString(), anyString());
    calls.verify(admin).jsonExecute(anyString());
    calls.verify(info).setString(BBjAdminContextInfo.JSON_DATA, running);
    calls.verify(info).commit();
    calls.verify(server).commit();
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldRemoveTheCreatedBbjShipContextWhenNothingAnswersOnThePort() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    contexts = new BBjShipContexts(admin, plugin, Duration.ofMillis(300));
    int port = WebServer.findClosedPort();
    int httpPort = port;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure =
        assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the HTTP port of the BBjServices web server", failure.getMessage());
    InOrder calls = inOrder(server);
    calls.verify(server).createContext("webforj-bbjship");
    calls.verify(server).removeContext("webforj-bbjship");
    calls.verify(server).commit();
  }

  @Test
  void shouldServeTheBbjShipContextOnTheHttpAndTheSslPort() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888}""");
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    int port = web.getPort();
    int sslPort = secure.getPort();

    try (web; secure) {
      contexts.enable(port, OptionalInt.of(sslPort), false);
    }

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "HTTP port": %3$d,
          "Path": "/webforj-bbjship",
          "Document Base": "%5$s/htdocs",
          "Protocol": true,
          "SSL port": %4$d,
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
        }""".formatted(folder, home, port, sslPort, runtime));
    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldRemoveTheCreatedBbjShipContextWhenNothingAnswersOnTheSslPort() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    contexts = new BBjShipContexts(admin, plugin, Duration.ofMillis(300));
    WebServer web = WebServer.startServingBBjShip();
    int port = WebServer.findClosedPort();
    int httpPort = web.getPort();
    OptionalInt sslPort = OptionalInt.of(port);

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));
    }

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the SSL port of the BBjServices web server", failure.getMessage());
    InOrder calls = inOrder(server);
    calls.verify(server).createContext("webforj-bbjship");
    calls.verify(server).removeContext("webforj-bbjship");
    calls.verify(server).commit();
  }

  @Test
  void shouldRestoreTheBbjShipContextWhenBbjServicesRefusesTheSslPort() throws Exception {
    Path documentRoot = home.resolve("plugins/.webforj-bbjship-deployments/htdocs");
    String running = RUNNING.formatted("webforj-bbjship", documentRoot);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    doThrow(new BBjAdminException(
        "8888 is not a valid SSL port. The following are valid ports: [8443]")).doNothing()
        .when(info).commit();
    OptionalInt sslPort = OptionalInt.of(8888);

    BBjAdminException failure =
        assertThrows(BBjAdminException.class, () -> contexts.enable(8080, sslPort, false));

    assertEquals("8888 is not a valid SSL port. The following are valid ports: [8443]",
        failure.getMessage());
    InOrder calls = inOrder(info, server, admin);
    calls.verify(info).setString(anyString(), anyString());
    calls.verify(info).commit();
    calls.verify(info).setString(BBjAdminContextInfo.JSON_DATA, running);
    calls.verify(info).commit();
    calls.verify(server).commit();
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
  }

  @Test
  void shouldRefuseContextOfAnotherApplication() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted("webforj-bbjship", "/srv/other/htdocs"));
    int httpPort = 8888;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure =
        assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));

    assertEquals("The BBjShip context belongs to another application", failure.getMessage());
    verify(info, never()).commit();
  }

  @Test
  void shouldRefuseMissingConfigurationFile() throws Exception {
    Files.delete(home.resolve("cfg/config.bbx"));
    int httpPort = 8888;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure =
        assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));

    assertEquals("The BBj configuration file is missing. Check the BBj installation",
        failure.getMessage());
  }

  @Test
  void shouldReportDocumentFolderThatCannotBeCreated() throws Exception {
    Path runtime = Files.createDirectories(home.resolve("plugins/.webforj-bbjship-deployments"));
    Files.writeString(runtime.resolve("htdocs"), "file");
    int httpPort = 8888;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure =
        assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));

    assertEquals("Cannot create the BBjShip document directory. Check the runtime folder",
        failure.getMessage());
  }

  @Test
  void shouldReportContextThatDoesNotStart() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", ""));
    when(admin.jsonExecute(anyString())).thenReturn("""
        {"MESSAGE": "Address already in use"}""");
    int httpPort = 8888;
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure =
        assertThrows(IOException.class, () -> contexts.enable(httpPort, sslPort, false));

    assertEquals("The BBjShip context did not start. Address already in use", failure.getMessage());
  }

  @Test
  void shouldRefuseServerWithoutWebContexts() throws Exception {
    BBjAdminWebAppServer serverWithoutContexts = mock(BBjAdminWebAppServer.class);
    when(admin.getWebAppServer()).thenReturn(serverWithoutContexts);

    IOException failure = assertThrows(IOException.class, contexts::disable);

    assertEquals("BBjServices does not manage web contexts. Update BBj", failure.getMessage());
  }

  private static Stream<String> getBBjShipChallenges() {
    return Stream.of("Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"",
        "Basic realm=\"webforj-bbjship\"");
  }
}
