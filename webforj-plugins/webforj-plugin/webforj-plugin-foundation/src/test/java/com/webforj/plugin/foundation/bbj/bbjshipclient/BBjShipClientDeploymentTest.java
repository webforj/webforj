package com.webforj.plugin.foundation.bbj.bbjshipclient;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

import com.sun.net.httpserver.HttpServer;
import com.webforj.plugin.foundation.bbj.BBjShipClient;
import com.webforj.plugin.foundation.bbj.HelloApp;
import com.webforj.plugin.foundation.bbj.model.BBjCredentials;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import javax.net.ssl.SSLException;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ClassicHttpRequest;
import org.apache.hc.core5.http.protocol.HttpContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Answers;
import org.mockito.MockedStatic;

class BBjShipClientDeploymentTest {

  private static final String VERSION_ANSWER = """
      {
        "version": "26.03"
      }
      """;
  private static final String DEPLOYMENT_ANSWER = """
      {
        "name": "hello-app",
        "url": "/webapp/hello-app"
      }
      """;
  private static final String APP_NAME = "hello-app";
  private static final String ARCHIVE_FILE = "hello-app.zip";
  private static final String LOOPBACK_ADDRESS = "127.0.0.1";
  private static final String VERSION_PATH = "/webforj-bbjship/version";
  private static final String CONTENT_TYPE = "Content-Type";
  private static final String JSON = "application/json";
  private static final String APP_PATH = "/webforj-bbjship/applications/hello-app"; // NOSONAR
  private static final String LOCATION = "Location";
  private static final String PUBLISHED_PATH = "/webapp/hello-app";
  private static final String ENDPOINT_FORMAT = "http://127.0.0.1:%d/webforj-bbjship";
  private static final String CLIENT_VERSION = "26.03";
  private static final String ADMIN_USERNAME = "administrator";
  private static final String ADMIN_PASSWORD = "secret-password";
  private static final String UPLOAD_COMPLETE =
      "Upload complete, waiting for BBj deployment confirmation";

  @TempDir
  Path temporary;

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldPutHelloAppArchiveWithCredentialsAndReturnThePublishedAddress(
      BBjCredentials credentials, String authorization) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    List<String> requests = new ArrayList<>();
    AtomicReference<byte[]> upload = new AtomicReference<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      upload.set(exchange.getRequestBody().readAllBytes());
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst(CONTENT_TYPE) + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = DEPLOYMENT_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.getResponseHeaders().set(LOCATION, PUBLISHED_PATH);
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint).setCredentials(credentials)
          .setVersion(CLIENT_VERSION).build();

      URI published = client.deploy(archive, APP_NAME);

      assertEquals(endpoint.resolve(PUBLISHED_PATH), published);
      assertEquals(
          List.of("PUT /webforj-bbjship/applications/hello-app application/zip " + authorization),
          requests);
      assertArrayEquals(Files.readAllBytes(archive), upload.get());
    } finally {
      bbjShip.stop(0);
    }
  }

  @ParameterizedTest
  @MethodSource("getRefusedDeployments")
  void shouldFailWithoutReportingSuccessWhenTheBbjShipRefusesOrDoesNotConfirm(int status,
      String contentType, String answer, String message) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.getResponseHeaders().set(CONTENT_TYPE, contentType);
      exchange.sendResponseHeaders(status, answer.isEmpty() ? -1 : answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).setLog(messages::add).build();
      String publishName = APP_NAME;

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(message, failure.getMessage());
      assertEquals(UPLOAD_COMPLETE, messages.getLast());
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldNameTheCertificateWhenTheHttpsConnectionFails() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    ServerSocket bbjShip = new ServerSocket(0, 1, InetAddress.getLoopbackAddress());
    Thread plainHttp = new Thread(() -> {
      try (Socket socket = bbjShip.accept()) {
        socket.getInputStream().read();
        socket.getOutputStream()
            .write("HTTP/1.1 400 Bad Request\r\n\r\n".getBytes(StandardCharsets.UTF_8));
      } catch (IOException closed) {
        return;
      }
    });
    plainHttp.start();
    URI endpoint =
        URI.create("https://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getLocalPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).build();
      String publishName = APP_NAME;

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertTrue(failure.getMessage().startsWith("BBj deployment could not establish a trusted "
          + "HTTPS connection to " + endpoint + ". Install a certificate for the host name of "
          + "the URL on the BBj host or add its certificate to the Java truststore of the build"),
          failure.getMessage());
    } finally {
      plainHttp.join(TimeUnit.SECONDS.toMillis(5));
      bbjShip.close();
    }
  }

  @Test
  void shouldNameTheCertificateWhenTheHttpsFailureHasNoDetail() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    CloseableHttpClient connection = mock(CloseableHttpClient.class);
    when(connection.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new SSLException((String) null));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(connection);
    URI endpoint = URI.create("https://build.example/webforj-bbjship");
    BBjShipClient client = BBjShipClient.create(endpoint)
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).build();
    String publishName = APP_NAME;

    try (MockedStatic<HttpClients> clients = mockStatic(HttpClients.class)) {
      clients.when(HttpClients::custom).thenReturn(connections);

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals("BBj deployment could not establish a trusted HTTPS connection to "
          + "https://build.example/webforj-bbjship. Install a certificate for the host "
          + "name of the URL on the BBj host or add its certificate to the Java truststore of "
          + "the build.", failure.getMessage());
    }
  }

  @Test
  void shouldRefuseMissingArchiveBeforeAnyRequest() {
    BBjShipClient client = BBjShipClient.create(URI.create("http://localhost:8888/webforj-bbjship"))
        .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
        .setVersion(CLIENT_VERSION).build();
    Path archive = temporary.resolve(ARCHIVE_FILE);
    String publishName = APP_NAME;

    IOException failure =
        assertThrows(IOException.class, () -> client.deploy(archive, publishName));

    assertEquals("BBj deployment archive is missing. Package the application first",
        failure.getMessage());
  }

  @Test
  void shouldReportUploadProgressOfHelloAppArchiveBeforeWaitingForConfirmation()
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    long size = Files.size(archive);
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = DEPLOYMENT_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.getResponseHeaders().set(LOCATION, PUBLISHED_PATH);
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).setLog(messages::add).build();

      client.deploy(archive, APP_NAME);

      assertEquals("Uploading BBj deployment, 0 of %d bytes (0%%)".formatted(size),
          messages.getFirst());
      assertEquals(List.of("Uploading BBj deployment, %d of %d bytes (100%%)".formatted(size, size),
          UPLOAD_COMPLETE), messages.subList(messages.size() - 2, messages.size()));
      Pattern progress =
          Pattern.compile("Uploading BBj deployment, (\\d+) of " + size + " bytes \\((\\d+)%\\)");
      long previous = -1;
      for (String line : messages.subList(0, messages.size() - 1)) {
        Matcher message = progress.matcher(line);
        assertTrue(message.matches(), line);
        long uploaded = Long.parseLong(message.group(1));
        assertTrue(uploaded > previous, line);
        assertEquals(uploaded * 100 / size, Long.parseLong(message.group(2)), line);
        previous = uploaded;
      }

      assertTrue(messages.size() > 3);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldReportProgressOfAnEmptyArchive() throws IOException {
    Path archive = Files.createFile(temporary.resolve("empty.zip"));
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = DEPLOYMENT_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.getResponseHeaders().set(LOCATION, PUBLISHED_PATH);
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).setLog(messages::add).build();

      client.deploy(archive, APP_NAME);

      assertEquals(List.of("Uploading BBj deployment, 0 of 0 bytes (0%)",
          "Uploading BBj deployment, 0 of 0 bytes (100%)", UPLOAD_COMPLETE), messages);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldReportWaitingWhileTheBbjShipHoldsTheConfirmation() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    CountDownLatch waiting = new CountDownLatch(1);
    CountDownLatch confirmation = new CountDownLatch(1);
    Consumer<String> log = message -> {
      if (UPLOAD_COMPLETE.equals(message)) {
        waiting.countDown();
      }
    };
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      try {
        confirmation.await();
      } catch (InterruptedException stopped) {
        Thread.currentThread().interrupt();
      }

      String answer = DEPLOYMENT_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.getResponseHeaders().set(LOCATION, PUBLISHED_PATH);
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));

    ExecutorService executor = Executors.newSingleThreadExecutor();
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION).setLog(log).build();

      Future<URI> deployment = executor.submit(() -> client.deploy(archive, APP_NAME));

      assertTrue(waiting.await(5, TimeUnit.SECONDS));
      assertFalse(deployment.isDone());
      confirmation.countDown();
      assertEquals(endpoint.resolve(PUBLISHED_PATH), deployment.get(5, TimeUnit.SECONDS));
    } finally {
      confirmation.countDown();
      bbjShip.stop(0);
      executor.shutdownNow();
    }
  }

  @Test
  void shouldKeepTheLogAfterRejectingNullLog() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve(APP_NAME));
    Path archive = HelloApp.writeArchive(app, temporary.resolve(ARCHIVE_FILE));
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress(LOOPBACK_ADDRESS, 0), 0);
    bbjShip.createContext(VERSION_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = VERSION_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext(APP_PATH, exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = DEPLOYMENT_ANSWER;
      exchange.getResponseHeaders().set(CONTENT_TYPE, JSON);
      exchange.getResponseHeaders().set(LOCATION, PUBLISHED_PATH);
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint = URI.create(ENDPOINT_FORMAT.formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient.Builder builder = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.createUser(ADMIN_USERNAME, ADMIN_PASSWORD))
          .setVersion(CLIENT_VERSION);
      Consumer<String> log = null;

      assertSame(builder, builder.setLog(messages::add));
      assertThrows(NullPointerException.class, () -> builder.setLog(log));

      builder.build().deploy(archive, APP_NAME);
      assertEquals(UPLOAD_COMPLETE, messages.getLast());
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

  private static Stream<Arguments> getRefusedDeployments() {
    String failed = "BBj deployment failed with HTTP %d. Check the endpoint, administration "
        + "credentials and BBjServices log";
    String unconfirmed = "BBj deployment did not confirm the requested application. Check the "
        + "BBjShip version and BBjServices log";
    String json = JSON;
    String problem = "application/problem+json";
    return Stream.of(Arguments.of(409, problem, """
        {
          "type": "about:blank",
          "title": "Conflict",
          "status": 409,
          "detail": "Choose another publish name"
        }
        """, "BBj deployment failed with HTTP 409. Choose another publish name"),
        Arguments.of(413, problem, """
            {
              "type": "about:blank",
              "title": "Content Too Large",
              "status": 413,
              "detail": "Shrink the archive"
            }
            """, "BBj deployment failed with HTTP 413. Shrink the archive"),
        Arguments.of(401, "text/plain", "denied", failed.formatted(401)),
        Arguments.of(302, json, "", failed.formatted(302)), Arguments.of(200, json, """
            {
              "name": "another",
              "url": "/webapp/another"
            }
            """, unconfirmed), Arguments.of(200, json, """
            {
              "name": "hello-app"
            }
            """, unconfirmed), Arguments.of(204, json, "", unconfirmed), Arguments.of(200, json,
            "x".repeat(65537), "BBj deployment response exceeds the allowed size"));
  }
}
