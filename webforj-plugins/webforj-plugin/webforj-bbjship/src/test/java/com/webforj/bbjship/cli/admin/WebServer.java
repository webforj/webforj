package com.webforj.bbjship.cli.admin;

import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsServer;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.atomic.AtomicInteger;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

/**
 * Serves loopback HTTP and HTTPS answers on a free port.
 */
public final class WebServer implements AutoCloseable {

  private static final String PROBLEM = "{\"title\": \"Unauthorized\", \"status\": 401}";
  private static final String CHALLENGE =
      "Basic realm=\"webforj-bbjship\", Bearer realm=\"webforj-bbjship\"";
  private static final char[] KEY_STORE_PASSWORD = "changeit".toCharArray();

  private final HttpServer server;
  private final int port;

  private WebServer(HttpServer server) {
    this(server, server.getAddress().getPort());
  }

  private WebServer(HttpServer server, int port) {
    this.server = server;
    this.port = port;
  }

  /**
   * Starts a server that answers like BBjShip.
   */
  public static WebServer startServingBBjShip() throws IOException {
    return startServingBBjShipWith(CHALLENGE, 0);
  }

  /**
   * Starts a server that answers like BBjShip over HTTPS with the self signed certificate of the
   * {@code fixtures/tls} key store.
   */
  public static WebServer startServingBBjShipOverHttps()
      throws IOException, GeneralSecurityException {
    KeyStore keys = KeyStore.getInstance("PKCS12");
    try (InputStream input =
        WebServer.class.getClassLoader().getResourceAsStream("fixtures/tls/localhost.p12")) {
      keys.load(input, KEY_STORE_PASSWORD); // NOSONAR
    }

    KeyManagerFactory managers =
        KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
    managers.init(keys, KEY_STORE_PASSWORD); // NOSONAR
    SSLContext context = SSLContext.getInstance("TLS");
    context.init(managers.getKeyManagers(), null, null);
    HttpsServer server =
        HttpsServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    server.setHttpsConfigurator(new HttpsConfigurator(context));

    return startServing(server, CHALLENGE, 0);
  }

  /**
   * Returns a loopback port nothing listens on.
   */
  public static int findClosedPort() throws IOException {
    try (ServerSocket socket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress())) {
      return socket.getLocalPort();
    }
  }

  /**
   * Returns the port the server listens on.
   */
  public int getPort() {
    return port;
  }

  /**
   * Stops the server.
   */
  @Override
  public void close() {
    server.stop(0);
  }

  static WebServer startServingBBjShipAfterClosing(int connections) throws IOException {
    CompletableFuture<Integer> bound = new CompletableFuture<>();
    HttpServer server = HttpServer.create();
    addBBjShipContexts(server, CHALLENGE, 0);
    Thread.ofPlatform().daemon().start(() -> {
      int port;
      try (ServerSocket closing = new ServerSocket()) {
        closing.setReuseAddress(true);
        closing.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0));
        port = closing.getLocalPort();
        bound.complete(port);
        for (int closed = 0; closed < connections; closed++) {
          closing.accept().close();
        }
      } catch (IOException failure) {
        bound.completeExceptionally(failure);
        throw new UncheckedIOException(failure);
      }

      try {
        server.bind(new InetSocketAddress(InetAddress.getLoopbackAddress(), port), 0);
        server.start();
      } catch (IOException failure) {
        throw new UncheckedIOException(failure);
      }
    });

    try {
      return new WebServer(server, bound.join());
    } catch (CompletionException failure) {
      throw new IOException(failure.getCause());
    }
  }

  static WebServer startServingBBjShipWith(String challenge, int notFoundAnswers)
      throws IOException {
    return startServing(
        HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0), challenge,
        notFoundAnswers);
  }

  static WebServer startServingOther() throws IOException {
    HttpServer server =
        HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    server.createContext("/", exchange -> {
      exchange.sendResponseHeaders(404, -1);
      exchange.close();
    });
    server.start();

    return new WebServer(server);
  }

  private static WebServer startServing(HttpServer server, String challenge, int notFoundAnswers) {
    addBBjShipContexts(server, challenge, notFoundAnswers);
    server.start();

    return new WebServer(server);
  }

  private static void addBBjShipContexts(HttpServer server, String challenge, int notFoundAnswers) {
    AtomicInteger notFound = new AtomicInteger(notFoundAnswers);
    server.createContext("/", exchange -> {
      exchange.sendResponseHeaders(404, -1);
      exchange.close();
    });
    server.createContext("/webforj-bbjship/version", exchange -> {
      if (notFound.getAndDecrement() > 0) {
        exchange.sendResponseHeaders(404, -1);
        exchange.close();
        return;
      }

      exchange.getResponseHeaders().set("Content-Type", "application/problem+json");
      exchange.getResponseHeaders().set("WWW-Authenticate", challenge);
      exchange.sendResponseHeaders(401, PROBLEM.length());
      exchange.getResponseBody().write(PROBLEM.getBytes(StandardCharsets.UTF_8));
      exchange.close();
    });
  }
}
