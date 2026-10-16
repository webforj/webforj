package com.webforj.bbjship.cli.admin;

import static com.webforj.bbjship.cli.command.BBjServices.ASSETS_CONTEXT;
import static com.webforj.bbjship.cli.command.BBjServices.BBJSHIP_CONTEXT;
import static com.webforj.bbjship.cli.command.Home.ASSETS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.DEPLOYMENTS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.HTDOCS_FOLDER;
import static com.webforj.bbjship.cli.command.Home.PLUGIN_FOLDER;
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

  private static final String ASSETS_EXECUTOR = "CONTEXT:webforj-bbjship-assets";
  private static final String ASSETS_RESTART = """
      {
        "EXECUTOR": "CONTEXT:webforj-bbjship-assets",
        "DATA": {
          "ACTION": "RESTART"
        },
        "ENABLED": true
      }""";

  @Test
  void shouldLeaveTheServerAloneWhenTheAssetsContextDoesNotExist() throws Exception {
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER)));
    WebServer web = WebServer.startServingBBjShip();

    try (web) {
      contexts.applyEndpoint(web.getPort(), OptionalInt.empty(), false);
    }

    verify(server, never()).getContextInfo(ASSETS_CONTEXT);
    verify(admin, never()).jsonExecute(contains(ASSETS_EXECUTOR));
  }

  @Test
  void shouldMoveTheAssetsContextToThePortsOfTheBbjShipContextAndRestartIt() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT, ASSETS_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER)));
    when(server.getContextInfo(ASSETS_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn("""
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(home.resolve(ASSETS_FOLDER)));
    WebServer web = WebServer.startServingBBjShip();
    WebServer secure = WebServer.startServingBBjShipOverHttps();
    int port = web.getPort();
    int sslPort = secure.getPort();

    try (web; secure) {
      contexts.applyEndpoint(port, OptionalInt.of(sslPort), false);
    }

    Path folder = home.resolve(PLUGIN_FOLDER);
    Path runtime = home.resolve(DEPLOYMENTS_FOLDER);
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
    calls.verify(admin).jsonExecute(ASSETS_RESTART);
  }

  @Test
  void shouldRestartTheAssetsContextOnThePortsOfTheBbjShipContextAsItIs() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT, ASSETS_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER)));
    when(server.getContextInfo(ASSETS_CONTEXT)).thenReturn(info);
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
        }""".formatted(port, home.resolve(ASSETS_FOLDER)));

    try (web) {
      contexts.applyEndpoint(port, OptionalInt.empty(), false);
    }

    verify(info, never()).setString(anyString(), anyString());
    verify(admin).jsonExecute(ASSETS_RESTART);
  }

  @Test
  void shouldLeaveTheAssetsContextStartedInThisSessionOnTheSamePortsAsItIs() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT, ASSETS_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER)));
    when(server.getContextInfo(ASSETS_CONTEXT)).thenReturn(info);
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
        }""".formatted(port, home.resolve(ASSETS_FOLDER)));

    try (web) {
      contexts.applyEndpoint(port, OptionalInt.empty(), true);
    }

    verify(info, never()).setString(anyString(), anyString());
    verify(admin, never()).jsonExecute(contains(ASSETS_EXECUTOR));
  }

  @Test
  void shouldRestartTheAssetsContextAsItIsWhenTheBbjShipContextFails() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    String running = RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER));
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT, ASSETS_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    when(server.getContextInfo(ASSETS_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(ASSETS_CONTEXT, home.resolve(ASSETS_FOLDER)));
    contexts = new BBjShipContexts(admin, plugin, Duration.ofMillis(300));
    WebServer web = WebServer.startServingOther();
    int port = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.applyEndpoint(port, sslPort, false));
    }

    assertEquals("The BBjServices web server does not answer on port " + port
        + ". Use the HTTP port of the BBjServices web server", failure.getMessage());
    verify(bbjShip).setString(BBjAdminContextInfo.JSON_DATA, running);
    verify(info, never()).setString(anyString(), anyString());
    verify(admin).jsonExecute(ASSETS_RESTART);
  }

  @Test
  void shouldRestoreBothContextsWhenTheMovedAssetsContextDoesNotRestart() throws Exception {
    BBjAdminContextInfo bbjShip = mock(BBjAdminContextInfo.class);
    String running = RUNNING.formatted(BBJSHIP_CONTEXT, home.resolve(HTDOCS_FOLDER));
    String assets = """
        {
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "HTTP port": 8888,
          "Protocol": true,
          "SSL port": "",
          "Document Base": "%s"
        }""".formatted(home.resolve(ASSETS_FOLDER));
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("root", BBJSHIP_CONTEXT, ASSETS_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(bbjShip);
    when(bbjShip.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(running);
    when(server.getContextInfo(ASSETS_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA)).thenReturn(assets);
    when(admin.jsonExecute(contains(ASSETS_EXECUTOR))).thenReturn("{\"MESSAGE\": \"Port in use\"}");
    WebServer web = WebServer.startServingBBjShip();
    int port = web.getPort();
    OptionalInt sslPort = OptionalInt.empty();

    IOException failure;
    try (web) {
      failure = assertThrows(IOException.class, () -> contexts.applyEndpoint(port, sslPort, false));
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
