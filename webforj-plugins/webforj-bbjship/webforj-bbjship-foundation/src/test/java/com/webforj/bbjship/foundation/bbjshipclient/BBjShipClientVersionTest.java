package com.webforj.bbjship.foundation.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.sun.net.httpserver.HttpServer;
import com.webforj.bbjship.foundation.BBjShipClient;
import com.webforj.bbjship.foundation.HelloApp;
import com.webforj.bbjship.foundation.model.BBjCredentials;
import com.webforj.bbjship.foundation.model.VersionResponse;
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

  private static final String OTHER_VERSION_ANSWER = """
      {
        "version": "26.02"
      }
      """;
  private static final String RUNTIME_ANSWER = """
      {
        "version": "26.02",
        "javaArguments": [
          "-Xmx1024m"
        ],
        "pid": 4242
      }
      """;
  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_FILE = "hello-app.zip";
  private static final String LOOPBACK_ADDRESS = "127.0.0.1";
  private static final String CONTENT_TYPE = "Content-Type";
  private static final String JSON = "application/json";
  private static final String ENDPOINT_FORMAT = "http://127.0.0.1:%d/webforj-bbjship";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String CLIENT_VERSION = "26.03";
  private static final String VERSION_REQUEST = "GET /webforj-bbjship/version";

  @TempDir
  Path temporary;

  @ParameterizedTest
  @MethodSource("getUnmatchedVersionAnswers")
  void shouldStopBeforeUploadWhenTheBbjShipAnswersAnotherVersion(String answer, String message)
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
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
      String publishName = APP_NAME;

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(message.formatted(endpoint), failure.getMessage());
      assertEquals(List.of(VERSION_REQUEST), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldNameSetupWhenTheBbjShipDoesNotAnswerTheVersion() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
      exchange.sendResponseHeaders(404, -1);
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).build();
      String publishName = APP_NAME;

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(
          "BBj deployment could not read the BBjShip version at " + endpoint + ", HTTP "
              + "404. Check the deployment URL and run webforj:bbj-setup on the BBj host",
          failure.getMessage());
      assertEquals(List.of(VERSION_REQUEST), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getRefusedCredentials")
  void shouldReportTheProblemDetailOfRefusedCredentialsFromTheVersionRequest(int status,
      String problem, String message) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.getResponseHeaders().set(CONTENT_TYPE, "application/problem+json");
      exchange.getResponseHeaders().set("WWW-Authenticate", "Basic realm=\"webforj-bbjship\"");
      exchange.sendResponseHeaders(status, problem.length());
      exchange.getResponseBody().write(problem.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).build();
      String publishName = APP_NAME;

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
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath());
      String answer = OTHER_VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
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
      String publishName = APP_NAME;

      IOException failure = assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals("BBjShip 26.02 at " + endpoint + " does not match webforJ 26.03. "
          + "Run webforj:bbj-setup on the BBj host", failure.getMessage());
      assertEquals(List.of(VERSION_REQUEST), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldSendTheCredentialsWithTheVersionRequest(BBjCredentials credentials,
      String authorization) throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = OTHER_VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint).setCredentials(credentials)
          .setVersion(CLIENT_VERSION).build();
      String publishName = APP_NAME;

      assertThrows(IOException.class, () -> client.undeploy(publishName));

      assertEquals(List.of("GET /webforj-bbjship/version " + authorization), requests);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldAnswerTheVersionAndTheJavaRuntimeOfTheBbjShip() throws IOException {
    List<String> requests = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = RUNTIME_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
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
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext("/", exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.sendResponseHeaders(404, -1);
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD)).build();

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
        Arguments.of(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD),
            "Basic YWRtaW5pc3RyYXRvcjpzZWNyZXQtcGFzc3dvcmQ="),
        Arguments.of(BBjCredentials.createToken("4f2a9c"), "Bearer 4f2a9c"));
  }

  private static Stream<Arguments> getUnmatchedVersionAnswers() {
    String unreadable = "BBj deployment could not read the BBjShip version at %s. "
        + "Run webforj:bbj-setup on the BBj host";
    return Stream.of(
        Arguments.of(OTHER_VERSION_ANSWER,
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
