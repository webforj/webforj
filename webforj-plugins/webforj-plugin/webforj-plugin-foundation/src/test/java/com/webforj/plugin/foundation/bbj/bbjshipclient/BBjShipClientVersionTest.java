package com.webforj.plugin.foundation.bbj.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.sun.net.httpserver.HttpServer;
import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.HelloApp;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import com.webforj.plugin.foundation.bbj.model.VersionResponse;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjShipClientVersionTest {

  @TempDir
  Path temporary;

  @ParameterizedTest
  @MethodSource("getUnmatchedVersionAnswers")
  void shouldStopBeforeUploadWhenTheBbjShipAnswersAnotherVersion(String answer, String message)
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
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
      String publishName = "hello-app";

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(message.formatted(endpoint), failure.getMessage());
      assertEquals(List.of("GET /webforj-bbjship/version"), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldNameSetupWhenTheBbjShipDoesNotAnswerTheVersion() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
      exchange.sendResponseHeaders(404, -1);
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

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(
          "BBj deployment could not read the BBjShip version at " + endpoint + ", HTTP "
              + "404. Check the deployment URL and run webforj:bbj-setup on the BBj host",
          failure.getMessage());
      assertEquals(List.of("GET /webforj-bbjship/version"), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getRefusedCredentials")
  void shouldReportTheProblemDetailOfRefusedCredentialsFromTheVersionRequest(int status,
      String problem, String message) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.getResponseHeaders().set("Content-Type", "application/problem+json");
      exchange.getResponseHeaders().set("WWW-Authenticate", "Basic realm=\"webforj-bbjship\"");
      exchange.sendResponseHeaders(status, problem.length());
      exchange.getResponseBody().write(problem.getBytes(StandardCharsets.UTF_8));
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

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(message, failure.getMessage());
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldStopBeforeUndeploymentWhenTheBbjShipAnswersAnotherVersion() throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
      String answer = """
          {
            "version": "26.02"
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
      String publishName = "hello-app";

      IOException failure = assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals("BBjShip 26.02 at " + endpoint + " does not match webforJ 26.03. "
          + "Run webforj:bbj-setup on the BBj host", failure.getMessage());
      assertEquals(List.of("GET /webforj-bbjship/version"), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldSendTheCredentialsWithTheVersionRequest(BBjCredentials credentials,
      String authorization) throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = """
          {
            "version": "26.02"
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
      BBjShipClient client =
          BBjShipClient.create(endpoint).setCredentials(credentials).setVersion("26.03").build();
      String publishName = "hello-app";

      assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals(List.of("GET /webforj-bbjship/version " + authorization), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldAnswerTheVersionAndTheJavaRuntimeOfTheBbjShip() throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = """
          {
            "version": "26.02",
            "javaArguments": [
              "-Xmx1024m"
            ],
            "pid": 4242
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

      VersionResponse answer = client.getVersion();

      assertEquals("26.02", answer.getVersion());
      assertEquals(List.of("-Xmx1024m"), answer.getJavaArguments());
      assertEquals(
          List.of(
              "GET /webforj-bbjship/version " + "Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQtcGFzc3dvcmQ="),
          requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldNameSetupWhenTheBbjShipDoesNotAnswerTheVersionCheck() throws IOException {
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.sendResponseHeaders(404, -1);
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).build();

      IOException failure = assertThrows(IOException.class, client::getVersion);

      assertEquals(
          "BBjShip check could not read the BBjShip version at " + endpoint + ", HTTP 404. "
              + "Check the deployment URL and run webforj:bbj-setup on the BBj host",
          failure.getMessage());
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

  private static Stream<Arguments> getUnmatchedVersionAnswers() {
    String unreadable = "BBj deployment could not read the BBjShip version at %s. "
        + "Run webforj:bbj-setup on the BBj host";
    return Stream.of(
        Arguments.of("""
            {
              "version": "26.02"
            }
            """,
            "BBjShip 26.02 at %s does not match webforJ 26.03. "
                + "Run webforj:bbj-setup on the BBj host"),
        Arguments.of("{}", unreadable), Arguments.of("", unreadable),
        Arguments.of("BBJSHIP_VERSION 26.03", unreadable));
  }

  private static Stream<Arguments> getRefusedCredentials() {
    return Stream.of(Arguments.of(401, """
        {
          "type": "about:blank",
          "title": "Unauthorized",
          "status": 401,
          "detail": "Check the BBj administration credentials or token"
        }
        """, "BBj deployment failed with HTTP 401. Check the BBj administration credentials or "
        + "token"));
  }
}
