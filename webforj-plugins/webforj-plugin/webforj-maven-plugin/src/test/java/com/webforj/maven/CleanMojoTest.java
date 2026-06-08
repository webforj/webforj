package com.webforj.maven;

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CleanMojoTest {

  private static CleanMojo mojo(Path tmp) {
    CleanMojo mojo = new CleanMojo();
    mojo.sourceRoot = tmp.resolve("src/main/frontend").toFile();

    return mojo;
  }

  @Test
  void shouldRemoveTheGeneratedDirectory(@TempDir Path tmp) throws Exception {
    Path generated = tmp.resolve("src/main/frontend/generated");
    Files.createDirectories(generated.resolve("react"));
    Files.writeString(generated.resolve("react/card.tsx"), "x");

    mojo(tmp).execute();

    assertFalse(Files.exists(generated), "the generated frontend is removed");
  }
}
