package com.webforj.bbjship.foundation.setup;

import com.google.gson.Gson;
import com.sun.net.httpserver.HttpServer;
import com.webforj.bbjship.foundation.ContainerOptionsFile;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

final class VersionServer implements AutoCloseable {

  enum RuntimeAnswer {
    OTHER_SETTINGS, HOTSWAP_SETTINGS, HOTSWAP_SETTINGS_AFTER_START, NO_ANSWER_AFTER_START
  }

  private final HttpServer server;
  private final AtomicInteger requestsAfterStart;

  private VersionServer(HttpServer server, AtomicInteger requestsAfterStart) {
    this.server = server;
    this.requestsAfterStart = requestsAfterStart;
  }

  static VersionServer start(Path home, Path optionsDirectory, RuntimeAnswer answer)
      throws IOException {
    HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    AtomicInteger requestsAfterStart = new AtomicInteger();
    server.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      boolean restarted = Files.exists(home.resolve("services-restarted"));
      if (restarted) {
        requestsAfterStart.incrementAndGet();
      }

      if (restarted && answer == RuntimeAnswer.NO_ANSWER_AFTER_START) {
        exchange.sendResponseHeaders(503, -1);
        exchange.close();
        return;
      }

      boolean hotswap = answer == RuntimeAnswer.HOTSWAP_SETTINGS
          || answer == RuntimeAnswer.HOTSWAP_SETTINGS_AFTER_START && restarted;
      List<String> arguments =
          hotswap ? ContainerOptionsFile.readLines(optionsDirectory) : List.of("-Xmx1024m");
      byte[] body = new Gson().toJson(Map.of("version", "26.03", "javaArguments", arguments))
          .getBytes(StandardCharsets.UTF_8);
      exchange.getResponseHeaders().set("Content-Type", "application/json");
      exchange.sendResponseHeaders(200, body.length);
      exchange.getResponseBody().write(body);
      exchange.close();
    });
    server.start();

    return new VersionServer(server, requestsAfterStart);
  }

  URI getUrl() {
    return URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(getPort()));
  }

  int getRequestsAfterStart() {
    return requestsAfterStart.get();
  }

  int getPort() {
    return server.getAddress().getPort();
  }

  @Override
  public void close() {
    server.stop(0);
  }
}
