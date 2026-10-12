package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminException;
import com.google.gson.JsonParser;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

final class BBjShipServletAccessTest extends AdministratorRequest {

  @ParameterizedTest
  @ValueSource(strings = {"Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=",
      "basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=",
      "  BASIC   YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=  "})
  void shouldOpenTheSessionWithBasicCredentialsSplitAtTheFirstColon(String authorization)
      throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getHeader("Authorization")).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");
    when(sessions.create("administrator", "secret:password")).thenReturn(admin);

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(sessions).create("administrator", "secret:password");
    verify(response).setStatus(200);
    verify(admin).release();
  }

  @ParameterizedTest
  @ValueSource(strings = {"Bearer c2VjcmV0", "bearer c2VjcmV0", "  BEARER   c2VjcmV0  "})
  void shouldOpenTheSessionWithTheBearerToken(String authorization) throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getHeader("Authorization")).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");
    when(sessions.create("c2VjcmV0")).thenReturn(admin);

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(sessions).create("c2VjcmV0");
    verify(sessions, never()).create(anyString(), anyString());
    verify(response).setStatus(200);
    verify(admin).release();
  }

  @ParameterizedTest
  @NullAndEmptySource
  @ValueSource(strings = {"Basic", "Bearer ", "Basic not-base64!", "Basic YWRtaW5pc3RyYXRvcg==",
      "Digest username=\"administrator\""})
  void shouldAnswerUnauthorizedWithTheBasicAndBearerChallenges(String authorization)
      throws Exception {
    when(request.getHeader("Authorization")).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader("WWW-Authenticate",
        "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"");
    verify(response).setContentType("application/problem+json");
    verify(sessions, never()).create(anyString(), anyString());
    verify(sessions, never()).create(anyString());
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Unauthorized",
          "status": 401,
          "detail": "Check the BBj administration credentials or token"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerUnauthorizedWhenTheServerRejectsTheCredentials() throws Exception {
    when(request.getHeader("Authorization")).thenReturn("Basic YWRtaW5pc3RyYXRvcjp3cm9uZw==");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");
    when(sessions.create("administrator", "wrong"))
        .thenThrow(new BBjAdminException("Invalid user or password"));

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader("WWW-Authenticate",
        "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"");
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Unauthorized",
          "status": 401,
          "detail": "Check the BBj administration credentials or token"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerUnauthorizedWhenTheServerRejectsTheToken() throws Exception {
    when(request.getHeader("Authorization")).thenReturn("Bearer ZXhwaXJlZA");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn("/version");
    when(sessions.create("ZXhwaXJlZA")).thenThrow(new BBjAdminException("Invalid token"));

    BBjShipServlet.handle(request, response, plugin, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader("WWW-Authenticate",
        "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"");
    verify(admin, never()).release();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Unauthorized",
          "status": 401,
          "detail": "Check the BBj administration credentials or token"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }
}
