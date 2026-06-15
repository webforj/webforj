package com.webforj.bundle.bun.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleEntryResolverTest {

  @Test
  void shouldReturnEmptyWhenRootMissing(@TempDir Path tmp) {
    assertTrue(new BundleEntryResolver().resolve(tmp.resolve("ghost"), Set.of("a.ts")).isEmpty());
  }

  @Test
  void shouldReturnEmptyWhenNoEntryPaths(@TempDir Path tmp) {
    assertTrue(new BundleEntryResolver().resolve(tmp, Set.of()).isEmpty());
  }

  @Test
  void shouldResolveLiteralPathsAnyLayout(@TempDir Path tmp) throws IOException {
    Files.createDirectories(tmp.resolve("card"));
    Files.createDirectories(tmp.resolve("theme"));
    Files.writeString(tmp.resolve("card").resolve("card.ts"), "");
    Files.writeString(tmp.resolve("theme").resolve("theme.css"), "");
    Files.writeString(tmp.resolve("unused.ts"), "");

    List<BundleEntryDeclaration> entries =
        new BundleEntryResolver().resolve(tmp, List.of("card/card.ts", "theme/theme.css"));

    assertEquals(2, entries.size());
    assertEquals("card/card.ts", entries.get(0).getSource());
    assertEquals(tmp.resolve("card").resolve("card.ts"), entries.get(0).getResolvedFile());
    assertEquals("theme/theme.css", entries.get(1).getSource());
  }

  @Test
  void shouldResolveTwoSameNamedFilesInDifferentFolders(@TempDir Path tmp) throws IOException {
    Files.createDirectories(tmp.resolve("a"));
    Files.createDirectories(tmp.resolve("b"));
    Files.writeString(tmp.resolve("a").resolve("index.ts"), "");
    Files.writeString(tmp.resolve("b").resolve("index.ts"), "");

    List<BundleEntryDeclaration> entries =
        new BundleEntryResolver().resolve(tmp, List.of("a/index.ts", "b/index.ts"));

    assertEquals(2, entries.size());
  }

  @Test
  void shouldReportUnresolvedExplicitPaths(@TempDir Path tmp) throws IOException {
    Files.writeString(tmp.resolve("present.ts"), "");

    List<String> missing =
        new BundleEntryResolver().getUnresolved(tmp, List.of("present.ts", "./missing/x.ts"));

    assertEquals(List.of("./missing/x.ts"), missing);
  }

  @Test
  void shouldTreatBareSpecifierAsNpmEntry(@TempDir Path tmp) {
    List<BundleEntryDeclaration> entries =
        new BundleEntryResolver().resolve(tmp, List.of("@ui5/webcomponents/dist/Button.js"));

    assertEquals(1, entries.size());
    assertTrue(entries.get(0).isNpm(), "a bare specifier must be an npm entry");
    assertEquals("@ui5/webcomponents/dist/Button.js", entries.get(0).getSource());
    assertEquals(null, entries.get(0).getResolvedFile());
  }

  @Test
  void shouldRejectPathsEscapingTheRoot(@TempDir Path tmp) throws IOException {
    Files.writeString(tmp.resolve("outside.ts"), "");
    Path nested = Files.createDirectories(tmp.resolve("inner"));

    assertTrue(new BundleEntryResolver().resolve(nested, List.of("../outside.ts")).isEmpty());
  }
}
