package com.webforj.bbjship.foundation.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.sun.net.httpserver.HttpServer;
import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipClientUndeploymentTest {

  private static final String VERSION_ANSWER = """
      {
        "version": "26.03"
      }
      """;
  private static final String LOOPBACK_ADDRESS = "127.0.0.1";
  private static final String VERSION_PATH = "/webforj-bbjship/version";
  private static final String CONTENT_TYPE = "Content-Type";
  private static final String ENDPOINT_FORMAT = "http://127.0.0.1:%d/webforj-bbjship";
  private static final String CLIENT_VERSION = "26.03";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String PROBLEM_JSON = "application/problem+json";

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldDeleteTheApplicationByNameWithCredentialsAndNoBody(BBjCredentials credentials,
      String authorization) throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, "application/json");
      exchange.sendResponseHeaders(200, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      byte[] body = exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Authorization") + " " + body.length);
      exchange.sendResponseHeaders(204, -1);
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint).setCredentials(credentials)
          .setVersion(CLIENT_VERSION).build();

      client.undeploy("hello-app");

      assertEquals(
          List.of("DELETE /webforj-bbjship/applications/hello-app " + authorization + " 0"),
          requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldSendPublishNameWithSlashesAsOneEncodedPathSegment() throws IOException {
    List<String> paths = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      paths.add(exchange.getRequestURI().getRawPath());
      String answer = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, "application/json");
      exchange.sendResponseHeaders(200, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).build();

      client.undeploy("../plugins/hello app");

      assertEquals(
          List.of(VERSION_PATH, "/webforj-bbjship/applications/..%2Fplugins%2Fhello%20app"), paths);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getRefusedUndeployments")
  void shouldFailWithoutFollowingRedirectWhenTheBbjShipRefuses(int status, String contentType,
      String answer, String message) throws IOException {
    List<String> paths = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      paths.add(exchange.getRequestURI().getPath());
      String body = answer;
      int code = status;
      if (exchange.getRequestURI().getPath().endsWith("/version")) {
        body = VERSION_ANSWER;
        code = 200;
      } else {
        exchange.getResponseHeaders().set("Location", "/redirected");
      }

      exchange.getResponseHeaders().set(CONTENT_TYPE, contentType);
      exchange.sendResponseHeaders(code, body.isEmpty() ? -1 : body.length());
      exchange.getResponseBody().write(body.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).build();
      String publishName = "hello-app";

      IOException failure = assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals(message, failure.getMessage());
      assertEquals(List.of(VERSION_PATH, "/webforj-bbjship/applications/hello-app"), paths);
    } finally {
      bbjShip.stop(0);
    }
  }

  private static Stream<Arguments> getCredentials() {
    return Stream.of(
        Arguments.of(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD),
            "Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQtcGFzc3dvcmQ="),
        Arguments.of(BBjCredentials.createToken("4f2a9c"), "Bearer 4f2a9c"));
  }

  private static Stream<Arguments> getRefusedUndeployments() {
    String failed = "BBj undeployment failed with HTTP %d. Check the endpoint, administration "
        + "credentials and BBjServices log";
    return Stream.of(
        Arguments.of(404, PROBLEM_JSON, """
            {
              "type": "about:blank",
              "title": "Not Found",
              "status": 404,
              "detail": "No BBj application named hello-app was deployed by BBjShip"
            }
            """,
            "BBj undeployment failed with HTTP 404. No BBj application named hello-app was "
                + "deployed by BBjShip"),
        Arguments.of(403, "text/plain", "denied", failed.formatted(403)),
        Arguments.of(302, "text/plain", "", failed.formatted(302)),
        Arguments.of(500, PROBLEM_JSON, "{}", failed.formatted(500)), Arguments.of(500,
            PROBLEM_JSON, "x".repeat(65537), "BBj undeployment response exceeds the allowed size"));
  }
}
