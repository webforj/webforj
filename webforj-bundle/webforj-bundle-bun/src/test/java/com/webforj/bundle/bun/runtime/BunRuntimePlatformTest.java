package com.webforj.bundle.bun.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Path;
import org.junit.jupiter.api.Test;

class BunRuntimePlatformTest {

  private static BunRuntime runtime() {
    return BunRuntime.create().setCacheRoot(Path.of("/tmp/cache")).setVersion("1.1.45")
        .setOverridePath(null).setReleaseHost("https://example.test/bun").build();
  }

  @Test
  void shouldResolveEveryKnownIntelArchToX64() {
    String original = System.getProperty("os.arch");
    try {
      for (String arch : new String[] {"amd64", "x86_64", "x64", "EM64T"}) {
        System.setProperty("os.arch", arch);
        assertTrue(runtime().getArchiveFileName().endsWith("-x64.zip"),
            arch + " should resolve to x64, got " + runtime().getArchiveFileName());
      }
    } finally {
      restore(original);
    }
  }

  @Test
  void shouldResolveEveryKnownArmArchToAarch64() {
    String original = System.getProperty("os.arch");
    try {
      for (String arch : new String[] {"aarch64", "arm64"}) {
        System.setProperty("os.arch", arch);
        assertTrue(runtime().getArchiveFileName().endsWith("-aarch64.zip"),
            arch + " should resolve to aarch64, got " + runtime().getArchiveFileName());
      }
    } finally {
      restore(original);
    }
  }

  @Test
  void shouldFailLoudlyWhenArchitectureBunDoesNotShip() {
    String original = System.getProperty("os.arch");
    try {
      System.setProperty("os.arch", "s390x");
      BunRuntime runtime = runtime();
      assertThrows(IllegalStateException.class, () -> runtime.getArchiveFileName());
    } finally {
      restore(original);
    }
  }

  @Test
  void shouldBuildArchiveUrlMatchingOfficialReleaseLayout() {
    BunRuntime runtime =
        BunRuntime.create().setCacheRoot(Path.of("/tmp/cache")).setVersion("1.1.45")
            .setOverridePath(null).setReleaseHost("https://example.test/bun").build();

    String url = runtime.getArchiveUrl();

    assertTrue(url.startsWith("https://example.test/bun/bun-v1.1.45/bun-"),
        "expected bun-v<version>/bun- pattern, got " + url);
    assertTrue(url.endsWith(".zip"));
  }

  @Test
  void shouldNameArchiveFileAsZip() {
    BunRuntime runtime =
        BunRuntime.create().setCacheRoot(Path.of("/tmp/cache")).setVersion("1.1.45")
            .setOverridePath(null).setReleaseHost("https://example.test/bun").build();

    assertTrue(runtime.getArchiveFileName().endsWith(".zip"));
  }

  @Test
  void shouldNameBinaryPerPlatform() {
    BunRuntime runtime =
        BunRuntime.create().setCacheRoot(Path.of("/tmp/cache")).setVersion("1.1.45")
            .setOverridePath(null).setReleaseHost("https://example.test/bun").build();

    String name = runtime.getBinaryName();

    boolean isWindows = System.getProperty("os.name", "").toLowerCase().contains("win");
    assertEquals(isWindows ? "bun.exe" : "bun", name);
  }

  private static void restore(String original) {
    if (original != null) {
      System.setProperty("os.arch", original);
    }
  }
}
