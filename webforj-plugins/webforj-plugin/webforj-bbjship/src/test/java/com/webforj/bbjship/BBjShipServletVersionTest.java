package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import org.junit.jupiter.api.Test;

final class BBjShipServletVersionTest extends AdministratorRequest {

  @Test
  void shouldAnswerTheVersionOfTheInstalledBbjShip() throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(200);
    verify(response).setContentType("application/json");
    verify(response).setContentLength(body.size());
    verify(response, never()).setHeader(anyString(), anyString());
    verify(admin).release();
    JsonObject answer =
        JsonParser.parseString(body.toString(StandardCharsets.UTF_8)).getAsJsonObject();
    assertEquals("26.03-SNAPSHOT", answer.get("version").getAsString());
    assertEquals(JSON.toJsonTree(ManagementFactory.getRuntimeMXBean().getInputArguments()),
        answer.get("javaArguments"));
    assertEquals(ProcessHandle.current().pid(), answer.get("pid").getAsLong());
    assertEquals(3, answer.size());
  }

  @Test
  void shouldAnswerInternalServerErrorWhenTheVersionIsBlank() throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"), "version=\n");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(500);
    verify(admin).release();
  }

  @Test
  void shouldAnswerInternalServerErrorWhenTheVersionCannotBeRead() throws Exception {
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(500);
    verify(admin).release();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Internal Server Error",
          "status": 500,
          "detail": "BBj deployment failed. Check the BBjServices log"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }
}
