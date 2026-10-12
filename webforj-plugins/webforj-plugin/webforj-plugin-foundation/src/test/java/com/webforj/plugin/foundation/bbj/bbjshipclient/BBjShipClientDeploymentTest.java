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

  @TempDir
  Path temporary;

  @ParameterizedTest
  @MethodSource("getCredentials")
  void shouldPutHelloAppArchiveWithCredentialsAndReturnThePublishedAddress(
      BBjCredentials credentials, String authorization) throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    List<String> requests = new ArrayList<>();
    AtomicReference<byte[]> upload = new AtomicReference<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      upload.set(exchange.getRequestBody().readAllBytes());
      requests.add(exchange.getRequestMethod() + " " + exchange.getRequestURI().getPath() + " "
          + exchange.getRequestHeaders().getFirst("Content-Type") + " "
          + exchange.getRequestHeaders().getFirst("Authorization"));
      String answer = """
          {
            "name": "hello-app",
            "url": "/webapp/hello-app"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.getResponseHeaders().set("Location", "/webapp/hello-app");
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client =
          BBjShipClient.create(endpoint).setCredentials(credentials).setVersion("26.03").build();

      URI published = client.deploy(archive, "hello-app");

      assertEquals(endpoint.resolve("/webapp/hello-app"), published);
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
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      exchange.getRequestBody().readAllBytes();
      exchange.getResponseHeaders().set("Content-Type", contentType);
      exchange.sendResponseHeaders(status, answer.isEmpty() ? -1 : answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .setLog(messages::add).build();
      String publishName = "hello-app";

      IOException failure =
          assertThrows(IOException.class, () -> client.deploy(archive, publishName));

      assertEquals(message, failure.getMessage());
      assertEquals("Upload complete, waiting for BBj deployment confirmation", messages.getLast());
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldNameTheCertificateWhenTheHttpsConnectionFails() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
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
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .build();
      String publishName = "hello-app";

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
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    CloseableHttpClient connection = mock(CloseableHttpClient.class);
    when(connection.execute(any(ClassicHttpRequest.class), any(HttpContext.class), any()))
        .thenThrow(new SSLException((String) null));
    HttpClientBuilder connections = mock(HttpClientBuilder.class, Answers.RETURNS_SELF);
    when(connections.build()).thenReturn(connection);
    URI endpoint = URI.create("https://build.example/webforj-bbjship");
    BBjShipClient client = BBjShipClient.create(endpoint)
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
        .build();
    String publishName = "hello-app";

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
        .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
        .build();
    Path archive = temporary.resolve("hello-app.zip");
    String publishName = "hello-app";

    IOException failure =
        assertThrows(IOException.class, () -> client.deploy(archive, publishName));

    assertEquals("BBj deployment archive is missing. Package the application first",
        failure.getMessage());
  }

  @Test
  void shouldReportUploadProgressOfHelloAppArchiveBeforeWaitingForConfirmation()
      throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    long size = Files.size(archive);
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = """
          {
            "name": "hello-app",
            "url": "/webapp/hello-app"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.getResponseHeaders().set("Location", "/webapp/hello-app");
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .setLog(messages::add).build();

      client.deploy(archive, "hello-app");

      assertEquals("Uploading BBj deployment, 0 of %d bytes (0%%)".formatted(size),
          messages.getFirst());
      assertEquals(
          List.of("Uploading BBj deployment, %d of %d bytes (100%%)".formatted(size, size),
              "Upload complete, waiting for BBj deployment confirmation"),
          messages.subList(messages.size() - 2, messages.size()));
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
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = """
          {
            "name": "hello-app",
            "url": "/webapp/hello-app"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.getResponseHeaders().set("Location", "/webapp/hello-app");
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .setLog(messages::add).build();

      client.deploy(archive, "hello-app");

      assertEquals(List.of("Uploading BBj deployment, 0 of 0 bytes (0%)",
          "Uploading BBj deployment, 0 of 0 bytes (100%)",
          "Upload complete, waiting for BBj deployment confirmation"), messages);
    } finally {
      bbjShip.stop(0);
    }
  }

  @Test
  void shouldReportWaitingWhileTheBbjShipHoldsTheConfirmation() throws Exception {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    CountDownLatch waiting = new CountDownLatch(1);
    CountDownLatch confirmation = new CountDownLatch(1);
    Consumer<String> log = message -> {
      if ("Upload complete, waiting for BBj deployment confirmation".equals(message)) {
        waiting.countDown();
      }
    };
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      exchange.getRequestBody().readAllBytes();
      try {
        confirmation.await(30, TimeUnit.SECONDS);
      } catch (InterruptedException stopped) {
        Thread.currentThread().interrupt();
      }

      String answer = """
          {
            "name": "hello-app",
            "url": "/webapp/hello-app"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.getResponseHeaders().set("Location", "/webapp/hello-app");
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));

    ExecutorService executor = Executors.newSingleThreadExecutor();
    try {
      BBjShipClient client = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password")).setVersion("26.03")
          .setLog(log).build();

      Future<URI> deployment = executor.submit(() -> client.deploy(archive, "hello-app"));

      assertTrue(waiting.await(5, TimeUnit.SECONDS));
      assertFalse(deployment.isDone());
      confirmation.countDown();
      assertEquals(endpoint.resolve("/webapp/hello-app"), deployment.get(5, TimeUnit.SECONDS));
    } finally {
      confirmation.countDown();
      bbjShip.stop(0);
      executor.shutdownNow();
    }
  }

  @Test
  void shouldKeepTheLogAfterRejectingNullLog() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path archive = HelloApp.writeArchive(app, temporary.resolve("hello-app.zip"));
    List<String> messages = new ArrayList<>();
    HttpServer bbjShip = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    bbjShip.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String version = """
          {
            "version": "26.03"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, version.length());
      exchange.getResponseBody().write(version.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.createContext("/webforj-bbjship/applications/hello-app", exchange -> {
      exchange.getRequestBody().readAllBytes();
      String answer = """
          {
            "name": "hello-app",
            "url": "/webapp/hello-app"
          }
          """;
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.getResponseHeaders().set("Location", "/webapp/hello-app");
      exchange.sendResponseHeaders(201, answer.length());
      exchange.getResponseBody().write(answer.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
    bbjShip.start();
    URI endpoint =
        URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(bbjShip.getAddress().getPort()));
    try {
      BBjShipClient.Builder builder = BBjShipClient.create(endpoint)
          .setCredentials(BBjCredentials.of("administrator", "secret-password"))
          .setVersion("26.03");
      Consumer<String> log = null;

      assertSame(builder, builder.setLog(messages::add));
      assertThrows(NullPointerException.class, () -> builder.setLog(log));

      builder.build().deploy(archive, "hello-app");
      assertEquals("Upload complete, waiting for BBj deployment confirmation", messages.getLast());
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

  private static Stream<Arguments> getRefusedDeployments() {
    String failed = "BBj deployment failed with HTTP %d. Check the endpoint, administration "
        + "credentials and BBjServices log";
    String unconfirmed = "BBj deployment did not confirm the requested application. Check the "
        + "BBjShip version and BBjServices log";
    String json = "application/json";
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
