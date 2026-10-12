package com.webforj.bbjship.cli.admin;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminWebAppServer_1500;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import com.webforj.bbjship.cli.command.Home;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;

/**
 * Holds an administration session whose web application server has no BBjShip contexts.
 */
abstract class EmptyWebAppServer {

  /**
   * Holds the data of a context BBjServices just created, formatted with its name and base.
   */
  static final String CREATED = """
      {
        "STOP": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "STOP"
          },
          "ENABLED": false
        },
        "START": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "START"
          },
          "ENABLED": true
        },
        "RESTART": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": false
        },
        "Auto Start": true,
        "HTTP port": 8888,
        "Name": "%1$s",
        "Path": "",
        "Document Base": "%2$s",
        "SSCP": "",
        "BBx Servlets": [],
        "Welcome Files": [
          "index.html",
          "index.htm",
          "index.bbjsp"
        ]
      }""";

  /**
   * Holds the data of a running context, formatted with its name and base.
   */
  static final String RUNNING = """
      {
        "STOP": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "STOP"
          },
          "ENABLED": true
        },
        "START": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "START"
          },
          "ENABLED": false
        },
        "RESTART": {
          "EXECUTOR": "CONTEXT:%1$s",
          "DATA": {
            "ACTION": "RESTART"
          },
          "ENABLED": true
        },
        "Name": "%1$s",
        "Document Base": "%2$s"
      }""";

  /**
   * Holds the temporary folder of the test.
   */
  @TempDir
  Path temporary;

  /**
   * Holds the BBj home copy.
   */
  Path home;
  /**
   * Holds the BBjShip plugin of the BBj home copy.
   */
  BBjShipPlugin plugin;
  /**
   * Holds the mocked administration session.
   */
  BBjAdminBase admin;
  /**
   * Holds the mocked web application server.
   */
  BBjAdminWebAppServer_1500 server;
  /**
   * Holds the mocked context data.
   */
  BBjAdminContextInfo info;
  /**
   * Holds the contexts under test.
   */
  BBjShipContexts contexts;

  /**
   * Creates a server without BBjShip contexts.
   */
  @BeforeEach
  void initServerWithoutBBjShipContexts() throws Exception {
    home = Home.createCopy(temporary.resolve("bbx"));
    plugin = new BBjShipPlugin(home);
    admin = mock(BBjAdminBase.class);
    server = mock(BBjAdminWebAppServer_1500.class);
    info = mock(BBjAdminContextInfo.class);
    when(admin.getWebAppServer()).thenReturn(server);
    when(admin.jsonExecute(anyString())).thenReturn("{}");
    when(server.getContextNames()).thenReturn(new BBjAdminAdapterList<>(List.of("root")));
    contexts = new BBjShipContexts(admin, plugin);
  }
}
