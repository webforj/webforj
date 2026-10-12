package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.OptionalInt;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

class BBjShipContextsAssetsTest extends EmptyWebAppServer {

  @Test
  void shouldCreateTheAssetsContextOnTheDefaultPortAndStartIt() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship-assets")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));

    contexts.addAssets(OptionalInt.empty(), OptionalInt.empty());

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    InOrder calls = inOrder(created, server, admin);
    calls.verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
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
          "Auto Start": true,
          "HTTP port": 8888,
          "Name": "webforj-bbjship-assets",
          "Path": "/webforj",
          "Document Base": "%3$s/assets",
          "SSCP": "",
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
          ],
          "Welcome Files": [
            "index.html",
            "index.htm",
            "index.bbjsp"
          ]
        }""".formatted(folder, home, runtime));
    calls.verify(created).commit();
    calls.verify(server).commit();
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "START"
          },
          "ENABLED": true
        }""");
    assertTrue(Files.isDirectory(runtime.resolve("assets")));
  }

  @Test
  void shouldCreateTheAssetsContextOnThePortOfTheBbjShipContext() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 9090, "Document Base": "%s"}"""
        .formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    when(server.createContext("webforj-bbjship-assets")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888}""");
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));

    contexts.addAssets(OptionalInt.empty(), OptionalInt.empty());

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "HTTP port": 9090,
          "Path": "/webforj",
          "Document Base": "%3$s/assets",
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
        }""".formatted(folder, home, runtime));
  }

  @Test
  void shouldCreateTheAssetsContextOnTheSslPortOfTheCommand() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship-assets")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888}""");
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    int port = web.getPort();
    int sslPort = secure.getPort();

    try (web; secure) {
      contexts.addAssets(OptionalInt.of(port), OptionalInt.of(sslPort));
    }

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "HTTP port": %3$d,
          "Path": "/webforj",
          "Document Base": "%5$s/assets",
          "Protocol": true,
          "SSL port": %4$d,
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
        }""".formatted(folder, home, port, sslPort, runtime));
    verify(server, never()).removeContext(anyString());
  }

  @Test
  void shouldRemoveTheAssetsContextWhenNothingAnswersOnTheSslPort() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    when(server.createContext("webforj-bbjship-assets")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));
    WebServer web = WebServer.startServingBBjShip();
    int port = WebServer.findClosedPort();
    OptionalInt httpPort = OptionalInt.of(web.getPort());
    OptionalInt sslPort = OptionalInt.of(port);

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.addAssets(httpPort, sslPort));
    }

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the SSL port of the BBjServices web server", failure.getMessage());
    verify(server).removeContext("webforj-bbjship-assets");
  }

  @Test
  void shouldCreateTheAssetsContextOnTheSslPortOfTheBbjShipContext() throws Exception {
    BBjAdminContextInfo created = mock(BBjAdminContextInfo.class);
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8888, "SSL port": 8443, "Document Base": "%s"}"""
        .formatted(home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    when(server.createContext("webforj-bbjship-assets")).thenReturn(created);
    when(created.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {"HTTP port": 8080}""");
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship-assets", ""));

    contexts.addAssets(OptionalInt.empty(), OptionalInt.empty());

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    verify(created).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "HTTP port": 8888,
          "Path": "/webforj",
          "Document Base": "%3$s/assets",
          "SSL port": 8443,
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
        }""".formatted(folder, home, runtime));
  }

  @Test
  void shouldLeaveTheAssetsContextTheBbjShipOwnsAsItIs() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING.formatted(
        "webforj-bbjship-assets", home.resolve("plugins/.webforj-bbjship-deployments/assets")));

    contexts.addAssets(OptionalInt.of(8443), OptionalInt.empty());

    verify(info, never()).setString(anyString(), anyString());
    verify(server, never()).commit();
    verify(admin, never()).jsonExecute(anyString());
  }

  @Test
  void shouldRefuseMissingServlet() throws Exception {
    Files.delete(home.resolve("plugins/webforj-bbjship/servlets/WebforjBBjAssetsServlet.bbj"));

    OptionalInt httpPort = OptionalInt.of(8888);
    OptionalInt sslPort = OptionalInt.empty();
    IOException failure =
        assertThrows(IOException.class, () -> contexts.addAssets(httpPort, sslPort));

    assertEquals("The assets servlet is missing. Check the plugin installation",
        failure.getMessage());
  }
}
