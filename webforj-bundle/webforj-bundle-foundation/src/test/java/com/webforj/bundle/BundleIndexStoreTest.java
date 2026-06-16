package com.webforj.bundle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleIndexStoreTest {

  @AfterEach
  void resetCache() {
    BundleIndexStore.reset();
  }

  @Test
  void shouldReadTheIndexFromTheContextClassloader(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["view.js"]}}
        """);

    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(loader(tmp));
    try {
      BundleIndexStore.reset();

      assertEquals(List.of("view.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldReadTheDevelopmentIndexFreshOnEveryCall(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["app.css"]}}
        """);

    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(loader(tmp));
    try {
      BundleIndexStore.reset();
      assertEquals(List.of("app.css"), BundleIndexStore.get().getBindings().get("View"));

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["app.css","styles.scss"]}}
          """);

      assertEquals(List.of("app.css", "styles.scss"),
          BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldPreferTheDevelopmentIndexOverTheBuiltIndex(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["built.js"]}}
        """);
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["dev.js"]}}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("dev.js"), index.getBindings().get("View"));
  }

  @Test
  void shouldFallBackToTheBuiltIndexWhenNoDevelopmentIndexIsPresent(@TempDir Path tmp)
      throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["view.js"]},"debug":["panel.js"]}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("view.js"), index.getBindings().get("View"));
    assertEquals(List.of("panel.js"), index.getDebugFiles());
  }

  @Test
  void shouldReadNullWhenNoFileIsPresent(@TempDir Path tmp) {
    assertNull(BundleIndexStore.read(loader(tmp)));
  }

  @Test
  void shouldReadTheEagerBundleFromTheClasspath(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"eager":["generated/bundle-A1B2.js"]}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("generated/bundle-A1B2.js"), index.getEagerFiles());
    assertTrue(index.getBindings().isEmpty());
  }

  private static void writeIndex(Path root, String resource, String json) throws IOException {
    Path target = root.resolve(resource);
    Files.createDirectories(target.getParent());
    Files.writeString(target, json);
  }

  private static ClassLoader loader(Path root) {
    try {
      return new URLClassLoader(new URL[] {root.toUri().toURL()},
          ClassLoader.getPlatformClassLoader());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
