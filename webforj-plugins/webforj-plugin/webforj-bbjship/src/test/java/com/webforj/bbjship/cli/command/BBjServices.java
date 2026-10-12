package com.webforj.bbjship.cli.command;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEFAULTS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.BBjAdminException;
import com.basis.api.admin.BBjAdminWebAppServer_1500;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Holds a mocked BBjServices administration session with its classpaths and contexts.
 */
public final class BBjServices {

  private static final String CREATED = """
      {
        "STOP": {"EXECUTOR": "CONTEXT:%1$s", "DATA": {"ACTION": "STOP"}, "ENABLED": false},
        "START": {"EXECUTOR": "CONTEXT:%1$s", "DATA": {"ACTION": "START"}, "ENABLED": true},
        "RESTART": {"EXECUTOR": "CONTEXT:%1$s", "DATA": {"ACTION": "RESTART"}, "ENABLED": false},
        "HTTP port": 8888,
        "Name": "%1$s"
      }""";

  private final Map<String, List<String>> classpaths = new LinkedHashMap<>();
  private final Map<String, BBjAdminContextInfo> contexts = new LinkedHashMap<>();
  private final BBjAdminBase admin = mock(BBjAdminBase.class);
  private final BBjAdminWebAppServer_1500 server = mock(BBjAdminWebAppServer_1500.class);

  /**
   * Creates the mocked administration session.
   */
  public BBjServices() throws BBjAdminException {
    when(admin.getClasspathNames())
        .thenAnswer(invocation -> new BBjAdminAdapterList<>(List.copyOf(classpaths.keySet())));
    when(admin.getClasspath(anyString())).thenAnswer(
        invocation -> new BBjAdminAdapterList<>(classpaths.get(invocation.getArgument(0))));
    doAnswer(invocation -> {
      List<String> entries = invocation.getArgument(1);
      if (entries == null) {
        classpaths.remove(invocation.getArgument(0));
      } else {
        classpaths.put(invocation.getArgument(0), entries);
      }

      return null;
    }).when(admin).setClasspath(anyString(), any());
    when(admin.getWebAppServer()).thenReturn(server);
    when(admin.jsonExecute(anyString())).thenReturn("{}");
    when(server.getContextNames())
        .thenAnswer(invocation -> new BBjAdminAdapterList<>(List.copyOf(contexts.keySet())));
    when(server.getContextInfo(anyString()))
        .thenAnswer(invocation -> contexts.get(invocation.getArgument(0)));
    when(server.createContext(anyString()))
        .thenAnswer(invocation -> addContext(invocation.getArgument(0),
            CREATED.formatted((String) invocation.getArgument(0))));
    doAnswer(invocation -> contexts.remove(invocation.getArgument(0))).when(server)
        .removeContext(anyString());
  }

  /**
   * Returns the mocked administration session.
   */
  public BBjAdminBase getAdmin() {
    return admin;
  }

  /**
   * Returns the mocked web application server.
   */
  public BBjAdminWebAppServer_1500 getServer() {
    return server;
  }

  /**
   * Returns the context with the given name, or null when it does not exist.
   */
  public BBjAdminContextInfo getContext(String name) {
    return contexts.get(name);
  }

  /**
   * Adds a classpath with the given entries.
   */
  public void addClasspath(String name, List<String> entries) {
    classpaths.put(name, entries);
  }

  BBjAdminContextInfo addContext(String name, String data) {
    AtomicReference<String> stored = new AtomicReference<>(data);
    BBjAdminContextInfo info = mock(BBjAdminContextInfo.class, invocation -> {
      if ("getString".equals(invocation.getMethod().getName())) {
        return stored.get();
      }

      if ("setString".equals(invocation.getMethod().getName())) {
        stored.set(invocation.getArgument(1));
      }

      return RETURNS_DEFAULTS.answer(invocation);
    });
    contexts.put(name, info);

    return info;
  }

  /**
   * Adds the classpaths and the assets context BBjShip expects in the given BBj home.
   */
  public void addBBjShipConfiguration(Path home) {
    addClasspath("_webforj_bbjship_cli", List.of("(bbj_default)",
        home.resolve("plugins/webforj-bbjship/lib/webforj-bbjship.jar").toString()));
    addClasspath("_webforj_bbjship", List.of(home.resolve(".lib/BBj.jar").toString(),
        home.resolve(".lib/BBjUtil.jar").toString()));
    addContext("webforj-bbjship-assets", """
        {
          "START": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "START"},
            "ENABLED": false},
          "RESTART": {"EXECUTOR": "CONTEXT:webforj-bbjship-assets", "DATA": {"ACTION": "RESTART"},
            "ENABLED": true},
          "Document Base": "%s"
        }""".formatted(home.resolve("plugins/.webforj-bbjship-deployments/assets")));
  }
}
