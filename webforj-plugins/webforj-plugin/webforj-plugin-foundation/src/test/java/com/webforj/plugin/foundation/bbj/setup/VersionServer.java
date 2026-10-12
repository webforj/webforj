package com.webforj.plugin.foundation.bbj.setup;

import com.google.gson.Gson;
import com.sun.net.httpserver.HttpServer;
import com.webforj.plugin.foundation.hotswap.ContainerOptionsFile;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

final class VersionServer implements AutoCloseable {

  enum RuntimeAnswer {
    OTHER_SETTINGS, HOTSWAP_SETTINGS, HOTSWAP_SETTINGS_AFTER_START
  }

  private final HttpServer server;

  private VersionServer(HttpServer server) {
    this.server = server;
  }

  static VersionServer start(Path home, Path optionsDirectory, RuntimeAnswer answer)
      throws IOException {
    HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    server.createContext("/webforj-bbjship/version", exchange -> {
      exchange.getRequestBody().readAllBytes();
      boolean restarted = Files.exists(home.resolve("services-restarted"));
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

    return new VersionServer(server);
  }

  URI getUrl() {
    return URI.create("http://127.0.0.1:%d/webforj-bbjship".formatted(getPort()));
  }

  int getPort() {
    return server.getAddress().getPort();
  }

  @Override
  public void close() {
    server.stop(0);
  }
}
