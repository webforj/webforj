package com.webforj.devtools.craftforj.docs.fetcher;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.sun.net.httpserver.HttpServer;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class DwcFetcherTest {

  private static final String VALID_BODY = """
      {"components":[{"tag":"dwc-button","parts":[{"name":"control","docs":"d"}]}]}""";

  private final AtomicInteger hits = new AtomicInteger();
  private HttpServer server;

  private DwcFetcher fetcherFor(int status, String body) throws IOException {
    server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
    server.createContext("/docs", exchange -> {
      hits.incrementAndGet();
      byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
      exchange.sendResponseHeaders(status, bytes.length);
      exchange.getResponseBody().write(bytes);
      exchange.close();
    });
    server.start();

    return new DwcFetcher("http://127.0.0.1:" + server.getAddress().getPort() + "/docs");
  }

  @AfterEach
  void stopServer() {
    if (server != null) {
      server.stop(0);
    }
  }

  @Test
  @DisplayName("Should fetch and parse component styling data")
  void shouldFetchAndParse() throws IOException {
    DwcFetcher fetcher = fetcherFor(200, VALID_BODY);

    DwcStylingData data = fetcher.fetch("dwc-button");

    assertNotNull(data);
    assertEquals(1, data.getParts().size());
    assertEquals("control", data.getParts().get(0).getName());
  }

  @Test
  @DisplayName("Should serve repeated lookups from the cache")
  void shouldCacheLookups() throws IOException {
    DwcFetcher fetcher = fetcherFor(200, VALID_BODY);

    fetcher.fetch("dwc-button");
    fetcher.fetch("dwc-button");
    fetcher.fetch("dwc-unknown");
    fetcher.fetch("dwc-unknown");

    assertEquals(1, hits.get());
  }

  @Test
  @DisplayName("Should return null on a non-200 response without hammering the server")
  void shouldReturnNullOnHttpError() throws IOException {
    DwcFetcher fetcher = fetcherFor(500, "boom");

    assertNull(fetcher.fetch("dwc-button"));
    assertNull(fetcher.fetch("dwc-button"));

    assertEquals(1, hits.get());
  }

  @Test
  @DisplayName("Should return null on malformed JSON")
  void shouldReturnNullOnMalformedJson() throws IOException {
    DwcFetcher fetcher = fetcherFor(200, "{not json");

    assertNull(fetcher.fetch("dwc-button"));
  }

  @Test
  @DisplayName("Should refetch after clearCache")
  void shouldRefetchAfterClearCache() throws IOException {
    DwcFetcher fetcher = fetcherFor(200, VALID_BODY);

    fetcher.fetch("dwc-button");
    fetcher.clearCache();
    fetcher.fetch("dwc-button");

    assertEquals(2, hits.get());
  }
}
