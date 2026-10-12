package com.webforj.bbjship.cli.admin;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminContextInfo;
import com.basis.api.admin.rest.adapter.BBjAdminAdapterList;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

class BBjShipContextsDisableTest extends EmptyWebAppServer {

  @Test
  void shouldStopAndRemoveTheRunningBbjShipContext() throws Exception {
    Path documentRoot = home.resolve("plugins/.webforj-bbjship-deployments/htdocs");
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted("webforj-bbjship", documentRoot));

    contexts.disable();

    InOrder calls = inOrder(admin, server);
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "STOP"
          },
          "ENABLED": true
        }""");
    calls.verify(server).removeContext("webforj-bbjship");
    calls.verify(server).commit();
  }

  @Test
  void shouldRemoveStoppedContextWithoutStoppingIt() throws Exception {
    Path documentRoot = home.resolve("plugins/.webforj-bbjship-deployments/htdocs");
    when(server.getContextNames())
        .thenReturn(new BBjAdminAdapterList<>(List.of("webforj-bbjship")));
    when(server.getContextInfo("webforj-bbjship")).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted("webforj-bbjship", documentRoot));

    contexts.disable();

    verify(admin, never()).jsonExecute(anyString());
    verify(server).removeContext("webforj-bbjship");
  }

  @Test
  void shouldLeaveTheServerAloneWhenTheContextDoesNotExist() throws Exception {
    contexts.disable();

    verify(server, never()).removeContext(anyString());
    verify(server, never()).commit();
  }
}
