package com.webforj.bbjship.cli.admin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.OptionalInt;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

class BBjShipContextsEnableAssetsTest extends EmptyWebAppServer {

  @Test
  void shouldLeaveTheServerAloneWhenTheAssetsContextDoesNotExist() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING
        .formatted("webforj-bbjship", home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    WebServer web = WebServer.startServingBBjShip();

    try (web) {
      contexts.enable(web.getPort(), OptionalInt.empty(), false);
    }

    verify(server, never()).getContextInfo("webforj-bbjship-assets");
    verify(admin, never()).jsonExecute(contains("CONTEXT:webforj-bbjship-assets"));
  }

  @Test
  void shouldMoveTheAssetsContextToThePortsOfTheBbjShipContextAndRestartIt() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames()).thenReturn(
        new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING
        .formatted("webforj-bbjship", home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/assets")));
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    int port = web.getPort();
    int sslPort = secure.getPort();

    try (web; secure) {
      contexts.enable(port, OptionalInt.of(sslPort), false);
    }

    Path folder = home.resolve("plugins/webforj-bbjship");
    Path runtime = home.resolve("plugins/.webforj-bbjship-deployments");
    InOrder calls = inOrder(info, server, admin);
    calls.verify(info).setString(BBjAdminContextInfo.JSON_DATA, """
        {
          "RESTART": {
            "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
            "DATA": {
              "ACTION": "RESTART"
            },
            "ENABLED": true
          },
          "HTTP port": %3$d,
          "Protocol": true,
          "SSL port": %4$d,
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
        }""".formatted(folder, home, port, sslPort, runtime));
    calls.verify(info).commit();
    calls.verify(server).commit();
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
  }

  @Test
  void shouldRestartTheAssetsContextOnThePortsOfTheBbjShipContextAsItIs() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames()).thenReturn(
        new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING
        .formatted("webforj-bbjship", home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": %d,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(port, home.resolve("plugins/.webforj-bbjship-deployments/assets")));

    try (web) {
      contexts.enable(port, OptionalInt.empty(), false);
    }

    verify(info, never()).setString(anyString(), anyString());
    verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
  }

  @Test
  void shouldLeaveTheAssetsContextStartedInThisSessionOnTheSamePortsAsItIs() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames()).thenReturn(
        new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING
        .formatted("webforj-bbjship", home.resolve("plugins/.webforj-bbjship-deployments/htdocs")));
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": %d,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(port, home.resolve("plugins/.webforj-bbjship-deployments/assets")));

    try (web) {
      contexts.enable(port, OptionalInt.empty(), true);
    }

    verify(info, never()).setString(anyString(), anyString());
    verify(admin, never()).jsonExecute(contains("CONTEXT:webforj-bbjship-assets"));
  }

  @Test
  void shouldRestartTheAssetsContextAsItIsWhenTheBbjShipContextFails() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    String running = RUNNING.formatted("webforj-bbjship",
        home.resolve("plugins/.webforj-bbjship-deployments/htdocs"));
    when(server.getContextNames()).thenReturn(
        new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(RUNNING.formatted(
        "webforj-bbjship-assets", home.resolve("plugins/.webforj-bbjship-deployments/assets")));
    contexts = new BBjShipContexts(admin, plugin, Duration.ofMillis(300));
    WebServer web = WebServer.startServingOther();
    int port = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.enable(port, sslPort, false));
    }

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the HTTP port of the BBjServices web server", failure.getMessage());
    verify(bbjShip).setString(BBjAdminContextInfo.JSON_DATA, running);
    verify(info, never()).setString(anyString(), anyString());
    verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        }""");
  }

  @Test
  void shouldRestoreBothContextsWhenTheMovedAssetsContextDoesNotRestart() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    String running = RUNNING.formatted("webforj-bbjship",
        home.resolve("plugins/.webforj-bbjship-deployments/htdocs"));
    String assets = """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/assets"));
    when(server.getContextNames()).thenReturn(
        new BBjAdminAdapterList<>(List.of("root", "webforj-bbjship", "webforj-bbjship-assets")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    when(server.getContextInfo("webforj-bbjship-assets")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(assets);
    when(admin.jsonExecute(contains("CONTEXT:webforj-bbjship-assets"))).thenReturn("""
        {"MESSAGE": "Port in use"}""");
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.enable(port, sslPort, false));
    }

    assertEquals("The assets context did not restart. Port in use", failure.getMessage());
    InOrder calls = inOrder(bbjShip, info, server);
    calls.verify(bbjShip).setString(anyString(), anyString());
    calls.verify(info).setString(anyString(), anyString());
    calls.verify(info).setString(BBjAdminContextInfo.JSON_DATA, assets);
    calls.verify(bbjShip).setString(BBjAdminContextInfo.JSON_DATA, running);
    verify(server, never()).removeContext(anyString());
  }
}
