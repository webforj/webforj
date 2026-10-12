package com.webforj.bbjship;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonParser;
import java.nio.charset.StandardCharsets;
import java.util.stream.Stream;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

final class BBjShipServletResourcesTest extends AdministratorRequest {

  @ParameterizedTest
  @MethodSource("getUnknownPaths")
  void shouldAnswerNotFoundForAnUnknownPath(String path) throws Exception {
    when(request.getMethod()).thenReturn("GET");
    when(request.getPathInfo()).thenReturn(path);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(404);
    verify(response).setContentType("application/problem+json");
    verify(admin).release();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Not Found",
          "status": 404,
          "detail": "BBjShip has no resource at %s. Check the deployment URL"
        }""".formatted(path),
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @MethodSource("getWrongMethods")
  void shouldAnswerMethodNotAllowedWithTheAllowedMethods(String method, String path, String allow)
      throws Exception {
    when(request.getMethod()).thenReturn(method);
    when(request.getPathInfo()).thenReturn(path);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(405);
    verify(response).setHeader("Allow", allow);
    verify(response).setContentType("application/problem+json");
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Method Not Allowed",
          "status": 405,
          "detail": "Method %s does not apply to this resource. Use %s"
        }""".formatted(method, allow),
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @ValueSource(strings = {"..", ".", "-hello-app", "hello app", "con", "LPT1.zip", ".hidden"})
  void shouldAnswerBadRequestForAnInvalidPublishNameInThePath(String name) throws Exception {
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn("/applications/" + name);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    verify(response).setContentType("application/problem+json");
    verify(request, never()).getInputStream();
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "The publish name is invalid. Use a name that starts with a letter or digit, \
        holds only letters, digits, underscores, dots and hyphens, and is not a reserved device \
        name such as con or nul"
        }""", JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  @ParameterizedTest
  @ValueSource(strings = {"apps", "WebApp", "files", "hello.js"})
  void shouldNameTheReservationInTheDetailOfReservedPublishName(String name) throws Exception {
    when(request.getMethod()).thenReturn("PUT");
    when(request.getPathInfo()).thenReturn("/applications/" + name);

    BBjShipServlet.handle(request, response, plugin, ClasspathRelease.NONE, sessions);

    verify(response).setStatus(400);
    assertEquals("""
        {
          "type": "about:blank",
          "title": "Bad Request",
          "status": 400,
          "detail": "The publish name %s is reserved by BBjServices for its own web paths. \
        Choose another publish name"
        }""".formatted(name),
        JSON.toJson(JsonParser.parseString(body.toString(StandardCharsets.UTF_8))));
  }

  private static Stream<Arguments> getUnknownPaths() {
    return Stream.of(Arguments.of((String) null), Arguments.of("/"), Arguments.of("/deploy"),
        Arguments.of("/applications"), Arguments.of("/applications/"),
        Arguments.of("/applications/hello-app/files"), Arguments.of("/apps/hello-app"),
        Arguments.of("/applications/../plugins"), Arguments.of("/version/"));
  }

  private static Stream<Arguments> getWrongMethods() {
    return Stream.of(Arguments.of("POST", "/version", "GET"),
        Arguments.of("DELETE", "/version", "GET"),
        Arguments.of("GET", "/applications/hello-app", "PUT, DELETE"),
        Arguments.of("POST", "/applications/hello-app", "PUT, DELETE"));
  }
}
