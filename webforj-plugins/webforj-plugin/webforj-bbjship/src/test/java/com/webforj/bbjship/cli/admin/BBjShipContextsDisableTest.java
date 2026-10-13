package com.webforj.bbjship.cli.admin;

import static com.webforj.bbjship.cli.command.BBjServices.BBJSHIP_CONTEXT;
import static com.webforj.bbjship.cli.command.Home.HTDOCS_FOLDER;
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
    Path documentRoot = home.resolve(HTDOCS_FOLDER);
    when(server.getContextNames()).thenReturn(new BBjAdminAdapterList<>(List.of(BBJSHIP_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(RUNNING.formatted(BBJSHIP_CONTEXT, documentRoot));

    contexts.removeEndpoint();

    InOrder calls = inOrder(admin, server);
    calls.verify(admin).jsonExecute("""
        {
          "EXECUTOR": "CONTEXT:webforj-bbjship",
          "DATA": {
            "ACTION": "STOP"
          },
          "ENABLED": true
        }""");
    calls.verify(server).removeContext(BBJSHIP_CONTEXT);
    calls.verify(server).commit();
  }

  @Test
  void shouldRemoveStoppedContextWithoutStoppingIt() throws Exception {
    Path documentRoot = home.resolve(HTDOCS_FOLDER);
    when(server.getContextNames()).thenReturn(new BBjAdminAdapterList<>(List.of(BBJSHIP_CONTEXT)));
    when(server.getContextInfo(BBJSHIP_CONTEXT)).thenReturn(info);
    when(info.getString(BBjAdminContextInfo.JSON_DATA))
        .thenReturn(CREATED.formatted(BBJSHIP_CONTEXT, documentRoot));

    contexts.removeEndpoint();

    verify(admin, never()).jsonExecute(anyString());
    verify(server).removeContext(BBJSHIP_CONTEXT);
  }

  @Test
  void shouldLeaveTheServerAloneWhenTheContextDoesNotExist() throws Exception {
    contexts.removeEndpoint();

    verify(server, never()).removeContext(anyString());
    verify(server, never()).commit();
  }
}
