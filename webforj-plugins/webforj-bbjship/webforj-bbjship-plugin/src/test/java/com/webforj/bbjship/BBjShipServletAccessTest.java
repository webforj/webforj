package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.api.admin.BBjAdminBase;
import com.basis.api.admin.BBjAdminException;
import com.google.gson.JsonParser;
import jakarta.servlet.ServletInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

final class BBjShipServletAccessTest extends AdministratorRequest {

  private static final String AUTHORIZATION_HEADER = "Authorization";
  private static final String CHALLENGE_HEADER = "WWW-Authenticate";
  private static final String CHALLENGE =
      "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"";
  private static final String VERSION_PATH = "/version";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String UNAUTHORIZED_PROBLEM = """
      {
        "type": "about:blank",
        "title": "Unauthorized",
        "status": 401,
        "detail": "Check the BBj administration credentials or token"
      }""";
  private static final String FORBIDDEN_PROBLEM = """
      {
        "type": "about:blank",
        "title": "Forbidden",
        "status": 403,
        "detail": "The BBj user has no permission to perform administrative tasks. Use the \
      credentials of a BBj administrator"
      }""";

  @ParameterizedTest
  @ValueSource(strings = {"Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=",
      "basic YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=",
      "  BASIC   YWRtaW5pc3RyYXRvcjpzZWNyZXQ6cGFzc3dvcmQ=  "})
  void shouldOpenTheSessionWithBasicCredentialsSplitAtTheFirstColon(String authorization)
      throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getHeader(AUTHORIZATION_HEADER)).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);
    doReturn(admin).when(sessions)
        .create(argThat(login -> ADMIN_USERNAME.equals(login.getUsername())
            && "secret:password".equals(login.getPassword())));

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(sessions).create(argThat(login -> ADMIN_USERNAME.equals(login.getUsername())
        && "secret:password".equals(login.getPassword())));
    verify(response).setStatus(200);
    verify(admin).release();
  }

  @ParameterizedTest
  @ValueSource(strings = {"Bearer c2VjcmV0", "bearer c2VjcmV0", "  BEARER   c2VjcmV0  "})
  void shouldOpenTheSessionWithTheBearerToken(String authorization) throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getHeader(AUTHORIZATION_HEADER)).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);
    doReturn(admin).when(sessions).create(argThat(login -> "c2VjcmV0".equals(login.getToken())));

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(sessions).create(argThat(login -> "c2VjcmV0".equals(login.getToken())));
    verify(sessions, never()).create(argThat(login -> !login.isToken()));
    verify(response).setStatus(200);
    verify(admin).release();
  }

  @ParameterizedTest
  @NullAndEmptySource
  @ValueSource(strings = {"Basic", "Bearer ", "Basic not-base64!", "Basic YWRtaW5pc3RyYXRvcg==",
      "Digest username=\"administrator\""})
  void shouldAnswerUnauthorizedWithTheBasicAndBearerChallenges(String authorization)
      throws Exception {
    when(request.getHeader(AUTHORIZATION_HEADER)).thenReturn(authorization);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader(CHALLENGE_HEADER, CHALLENGE);
    verify(response).setContentType("application/problem+json");
    verify(sessions, never()).create(any());
    assertEquals(UNAUTHORIZED_PROBLEM,
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerUnauthorizedWhenTheServerRejectsTheCredentials() throws Exception {
    when(request.getHeader(AUTHORIZATION_HEADER)).thenReturn("Basic YWRtaW5pc3RyYXRvcjp3cm9uZw==");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);
    doThrow(new BBjAdminException("Invalid user or password")).when(sessions)
        .create(argThat(login -> ADMIN_USERNAME.equals(login.getUsername())
            && "wrong".equals(login.getPassword())));

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader(CHALLENGE_HEADER, CHALLENGE);
    assertEquals(UNAUTHORIZED_PROBLEM,
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @Test
  void shouldAnswerUnauthorizedWhenTheServerRejectsTheToken() throws Exception {
    when(request.getHeader(AUTHORIZATION_HEADER)).thenReturn("Bearer ZXhwaXJlZA");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);
    doThrow(new BBjAdminException("Invalid token")).when(sessions)
        .create(argThat(login -> "ZXhwaXJlZA".equals(login.getToken())));

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(401);
    verify(response).setHeader(CHALLENGE_HEADER, CHALLENGE);
    verify(admin, never()).release();
    assertEquals(UNAUTHORIZED_PROBLEM,
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @MethodSource("getRequestsOfUserWithoutAdministratorPermission")
  void shouldAnswerForbiddenBeforeAnyWorkWhenTheUserIsNotAnAdministrator(String method, String path)
      throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getMethod()).thenReturn(method);
    when(request.getPathInfo()).thenReturn(path);
    when(request.getInputStream()).thenReturn(mock(ServletInputStream.class));
    when(admin.checkUserPermission(BBjAdminBase.PERMISSION_ALLOW_ADMIN)).thenReturn(false);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(403);
    verify(response).setContentType("application/problem+json");
    verify(response, never()).setHeader(anyString(), anyString());
    verify(request, never()).getInputStream();
    verify(admin, never()).getRemoteConfiguration();
    verify(admin, never()).commit();
    verify(admin).release();
    assertEquals(FORBIDDEN_PROBLEM,
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
    try (Stream<Path> staged = Files.list(runtime)) {
      assertEquals(List.of(), staged.toList());
    }
  }

  private static Stream<Arguments> getRequestsOfUserWithoutAdministratorPermission() {
    return Stream.of(Arguments.of("GET", VERSION_PATH),
        Arguments.of("PUT", "/applications/hello-app"),
        Arguments.of("DELETE", "/applications/hello-app"));
  }
}
