package com.webforj.plugin.foundation.bbj.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.sun.net.httpserver.HttpServer;
import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
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

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldDeleteTheApplicationByNameWithCredentialsAndNoBody(BBjCredentials credentials,
      String authorization) throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
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
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client =
          BBjShipClient.create(endpoint).setCredentials(credentials).setVersion("26.03").build();

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
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      paths.add(exchange.getRequestURI().getRawPath());
      String answer = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .build();

      client.undeploy("../plugins/hello app");

      assertEquals(List.of("/webforj-bbjship/version",
          "/webforj-bbjship/applications/..%2Fplugins%2Fhello%20app"), paths);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getRefusedUndeployments")
  void shouldFailWithoutFollowingRedirectWhenTheBbjShipRefuses(int status, String contentType,
      String answer, String message) throws IOException {
    List<String> paths = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      paths.add(exchange.getRequestURI().getPath());
      String body = answer;
      int code = status;
      if (exchange.getRequestURI().getPath().endsWith("/version")) {
        body = """
            {
              "version": "26.03"
            }
            """;
        code = 200;
      } else {
        exchange.getResponseHeaders().set("Location", "/redirected");
      }

      exchange.getResponseHeaders().set("Content-Type", contentType);
      exchange.sendResponseHeaders(code, body.isEmpty() ? -1 : body.length());
      exchange.getResponseBody().write(body.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .build();
      String publishName = "hello-app";

      IOException failure = assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals(message, failure.getMessage());
      assertEquals(List.of("/webforj-bbjship/version", "/webforj-bbjship/applications/hello-app"),
          paths);
    } finally {
      bbjShip.stop(0);
    }
  }

  private static Stream<Arguments> getCredentials() {
    return Stream.of(
        Arguments.of(BBjCredentials.of("administrator", "secret-password"),
            "Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQtcGFzc3dvcmQ="),
        Arguments.of(BBjCredentials.ofToken("4f2a9c"), "Bearer 4f2a9c"));
  }

  private static Stream<Arguments> getRefusedUndeployments() {
    String failed = "BBj undeployment failed with HTTP %d. Check the endpoint, administration "
        + "credentials and BBjServices log";
    return Stream.of(
        Arguments.of(404, "application/problem+json", """
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
        Arguments.of(500, "application/problem+json", "{}", failed.formatted(500)),
        Arguments.of(500, "application/problem+json", "x".repeat(65537),
            "BBj undeployment response exceeds the allowed size"));
  }
}
