package com.webforj.plugin.foundation;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class GeneratedFrontendTest {

  @Test
  void shouldResolveTheGeneratedDirectoryUnderTheSourceRoot(@TempDir Path tmp) {
    assertEquals(tmp.resolve("generated"), GeneratedFrontend.resolveDirectory(tmp));
  }

  @Test
  void shouldRemoveTheGeneratedTreeAndReportThePath(@TempDir Path tmp) throws Exception {
    Path generated = tmp.resolve("generated");
    Files.createDirectories(generated.resolve("react"));
    Files.writeString(generated.resolve("react/card.tsx"), "x");

    Optional<Path> removed = GeneratedFrontend.remove(tmp);

    assertTrue(removed.isPresent(), "the removed directory is reported");
    assertEquals(generated, removed.get());
    assertFalse(Files.exists(generated), "the generated tree is gone");
  }

  @Test
  void shouldReturnEmptyWhenThereIsNoGeneratedDirectory(@TempDir Path tmp) throws Exception {
    assertTrue(GeneratedFrontend.remove(tmp).isEmpty(), "nothing to remove");
  }
}
