package com.webforj.bundle.bun.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;
import org.junit.jupiter.api.Test;

class BunRuntimeIT {

  private static final List<String> PLATFORMS = List.of("darwin-aarch64", "darwin-x64",
      "linux-aarch64", "linux-x64", "windows-aarch64", "windows-x64");

  @Test
  void shouldPublishAnArchiveForEveryPlatformWebforjResolves() throws Exception {
    String version = System.getProperty("webforj.bundler.version", BunRuntime.DEFAULT_VERSION);
    BunRuntime current = BunRuntime.create().setVersion(version).build();
    assertTrue(
        PLATFORMS.contains(current.getArchiveFileName().replace("bun-", "").replace(".zip", "")),
        "the current platform must be one of the published archives");

    HttpClient client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.ALWAYS)
        .connectTimeout(Duration.ofSeconds(30)).build();
    for (String platform : PLATFORMS) {
      String url =
          BunRuntime.DEFAULT_RELEASE_HOST + "/bun-v" + version + "/bun-" + platform + ".zip";
      HttpRequest request = HttpRequest.newBuilder(URI.create(url))
          .method("HEAD", HttpRequest.BodyPublishers.noBody()).timeout(Duration.ofSeconds(30))
          .build();
      HttpResponse<Void> response = client.send(request, HttpResponse.BodyHandlers.discarding());

      assertEquals(200, response.statusCode(), "bun v" + version + " must publish " + url);
    }
  }
}
