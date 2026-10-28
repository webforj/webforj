package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
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
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
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
  private static final String UNENCRYPTED_PROBLEM = """
      {
        "type": "about:blank",
        "title": "Forbidden",
        "status": 403,
        "detail": "BBjShip refuses the administration credentials over an unencrypted connection \
      from another machine. Use the SSL port of the BBjServices web server"
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

  @ParameterizedTest
  @MethodSource("getUnencryptedRequestsFromAnotherMachine")
  void shouldRefuseUnencryptedRequestFromAnotherMachineBeforeReadingTheCredentials(String address,
      String method, String path) throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getRemoteAddr()).thenReturn(address);
    when(request.getMethod()).thenReturn(method);
    when(request.getPathInfo()).thenReturn(path);
    when(request.getInputStream()).thenReturn(mock(ServletInputStream.class));

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(403);
    verify(response).setContentType("application/problem+json");
    verify(response, never()).setHeader(anyString(), anyString());
    verify(request, never()).getHeader(anyString());
    verify(request, never()).getInputStream();
    verify(sessions, never()).create(any());
    assertEquals(UNENCRYPTED_PROBLEM,
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
    try (Stream<Path> staged = Files.list(runtime)) {
      assertEquals(List.of(), staged.toList());
    }
  }

  @ParameterizedTest
  @ValueSource(strings = {"127.0.0.1", "127.0.0.2", "0:0:0:0:0:0:0:1", "[0:0:0:0:0:0:0:1]", "::1",
      "[::1]", "::ffff:127.0.0.1"})
  void shouldAnswerUnencryptedRequestFromThisMachine(String address) throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getRemoteAddr()).thenReturn(address);
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(200);
    verify(admin).release();
  }

  @Test
  void shouldAnswerUnencryptedRequestFromAnAddressOfThisMachine() throws Exception {
    Optional<InetAddress> address = Collections.list(NetworkInterface.getNetworkInterfaces())
        .stream().flatMap(network -> Collections.list(network.getInetAddresses()).stream())
        .filter(candidate -> !candidate.isLoopbackAddress()).findFirst();
    assumeTrue(address.isPresent(), "This machine has no network interface address");
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.getRemoteAddr()).thenReturn(address.get().getHostAddress());
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(200);
    verify(admin).release();
  }

  @Test
  void shouldAnswerEncryptedRequestFromAnotherMachine() throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    when(request.isSecure()).thenReturn(true);
    when(request.getRemoteAddr()).thenReturn("192.168.1.20");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(200);
    verify(admin).release();
  }

  @Test
  void shouldAnswerUnencryptedRequestFromAnotherMachineWhenTheHostAllowsIt() throws Exception {
    Files.writeString(plugin.getDirectory().resolve("version.properties"),
        "version=26.03-SNAPSHOT\n");
    Files.writeString(runtime.resolve("unencrypted.enabled"),
        "unencrypted requests from another machine are allowed on this machine");
    when(request.getRemoteAddr()).thenReturn("192.168.1.20");
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(VERSION_PATH);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(200);
    verify(admin).release();
  }

  private static Stream<Arguments> getUnencryptedRequestsFromAnotherMachine() {
    return Stream.of(Arguments.of("192.168.1.20", "GET", VERSION_PATH),
        Arguments.of("2001:db8::10", "PUT", "/applications/hello-app"),
        Arguments.of("[2001:db8::10]", "PUT", "/applications/hello-app"),
        Arguments.of("build.example", "DELETE", "/applications/hello-app"),
        Arguments.of(null, "GET", VERSION_PATH));
  }

  private static Stream<Arguments> getRequestsOfUserWithoutAdministratorPermission() {
    return Stream.of(Arguments.of("GET", VERSION_PATH),
        Arguments.of("PUT", "/applications/hello-app"),
        Arguments.of("DELETE", "/applications/hello-app"));
  }
}
