package com.webforj.bbj.security.store;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.bbj.security.store.BBjEntryFile.Content;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class BBjEntryFileTest {
  private static final Instant NOW = Instant.parse("2026-01-01T00:00:00Z");
  private static final String LIVE = "t2." + NOW.plusSeconds(60).toEpochMilli() + "."
      + NOW.minusSeconds(60).toEpochMilli() + ".u." + "a".repeat(32);
  private static final String OTHER = "t2." + NOW.plusSeconds(120).toEpochMilli() + "."
      + NOW.toEpochMilli() + ".a." + "b".repeat(32);

  @TempDir
  private Path directory;

  private BBjEntryFile entryFile;
  private Path file;

  @BeforeEach
  void setUp() {
    entryFile = new BBjEntryFile(() -> directory, () -> "shop", Content.COUNTERS);
    file = directory.resolve("webforj-bbj-security").resolve("shop.counters");
  }

  @Nested
  class Names {

    @ParameterizedTest
    @MethodSource("hashCases")
    void shouldCreateTheLeadingBytesOfTheHashAsHex(int bytes, String expected) {
      assertEquals(expected, BBjEntryFile.createHash("abc", bytes));
    }

    private static Stream<Arguments> hashCases() {
      return Stream.of(Arguments.of(4, "ba7816bf"),
          Arguments.of(16, "ba7816bf8f01cfea414140de5dae2223"));
    }

    @ParameterizedTest
    @MethodSource("fileCases")
    void shouldResolveOneFilePerApplicationAndContent(Content content, String fileName) {
      BBjEntryFile target = new BBjEntryFile(() -> directory, () -> "shop", content);

      Path resolved = target.resolveFile();

      assertEquals(directory.resolve("webforj-bbj-security").resolve(fileName), resolved);
      assertSame(resolved, target.resolveFile());
    }

    private static Stream<Arguments> fileCases() {
      return Stream.of(Arguments.of(Content.SESSIONS, "shop.sessions"),
          Arguments.of(Content.COUNTERS, "shop.counters"));
    }

    @Test
    void shouldResolveTheApplicationKeyOnce() {
      assertEquals("shop", entryFile.resolveApplicationKey());
      assertSame(entryFile.resolveApplicationKey(), entryFile.resolveApplicationKey());
    }

    @ParameterizedTest
    @MethodSource("expiryCases")
    void shouldFindTheExpiryInTheNameOfAnEntry(String name, Optional<Instant> expected) {
      assertEquals(expected, entryFile.findExpiry(name));
    }

    private static Stream<Arguments> expiryCases() {
      return Stream.of(Arguments.of(LIVE, Optional.of(NOW.plusSeconds(60))),
          Arguments.of(null, Optional.empty()),
          Arguments.of("v1." + NOW.toEpochMilli() + "." + "h".repeat(43), Optional.empty()),
          Arguments.of("t2.1.1.x." + "a".repeat(32), Optional.empty()),
          Arguments.of("t1." + NOW.toEpochMilli() + ".u." + "a".repeat(32), Optional.empty()));
    }
  }

  @Nested
  class ReadAndWrite {

    @Test
    void shouldReadNoEntriesAndCreateNothingWithoutFile() throws IOException {
      assertTrue(entryFile.readEntries().isEmpty());
      assertFalse(Files.exists(file.getParent()));
    }

    @Test
    void shouldWriteTheChangedEntriesAndReadThemBack() throws IOException {
      entryFile.updateEntries(entries -> {
        entries.put(LIVE, "1");
        entries.put(OTHER, "2");
      });

      assertEquals(Map.of(LIVE, "1", OTHER, "2"), entryFile.readEntries());
      assertEquals(Optional.of("2"), entryFile.findEntry(OTHER));
      assertEquals(List.of(LIVE + "\t1", OTHER + "\t2"), Files.readAllLines(file));
    }

    @Test
    void shouldSkipLinesThatDoNotFitTheContent() throws IOException {
      Files.createDirectories(file.getParent());
      Files.write(file,
          List.of(LIVE + "\t1", "v1." + NOW.toEpochMilli() + "." + "h".repeat(43) + "\tAAAA",
              OTHER + "\tAAAA", "no entry", "t1." + NOW.plusSeconds(60).toEpochMilli() + ".u."
                  + "c".repeat(32) + "\t1." + NOW.toEpochMilli()));

      assertEquals(Map.of(LIVE, "1"), entryFile.readEntries());
    }

    @Test
    void shouldKeepTheFileWhenTheChangeFails() throws IOException {
      entryFile.updateEntries(entries -> entries.put(LIVE, "1"));
      Consumer<Map<String, String>> failing = entries -> {
        entries.clear();
        throw new IllegalStateException("change failed");
      };

      assertThrows(IllegalStateException.class, () -> entryFile.updateEntries(failing));

      assertEquals(List.of(LIVE + "\t1"), Files.readAllLines(file));
      try (Stream<Path> files = Files.list(file.getParent())) {
        assertEquals(List.of(file), files.toList());
      }
    }

    @Test
    void shouldRemoveOneEntry() throws IOException {
      entryFile.updateEntries(entries -> {
        entries.put(LIVE, "1");
        entries.put(OTHER, "2");
      });

      entryFile.removeEntry(LIVE);

      assertEquals(Map.of(OTHER, "2"), entryFile.readEntries());
    }

    @Test
    void shouldNotCreateTheFileToRemoveAnEntry() throws IOException {
      entryFile.removeEntry(LIVE);

      assertFalse(Files.exists(file.getParent()));
    }

    @Test
    void shouldShareTheFileBetweenInstancesOfOneApplication() throws IOException {
      BBjEntryFile other = new BBjEntryFile(() -> directory, () -> "shop", Content.COUNTERS);

      entryFile.updateEntries(entries -> entries.put(LIVE, "1"));

      assertEquals(Map.of(LIVE, "1"), other.readEntries());
    }
  }

  @Nested
  class RemoveStaleEntries {

    @Test
    void shouldRemoveTheExpiredEntriesAndKeepTheOthers() {
      String expired = "t2." + NOW.toEpochMilli() + "." + NOW.minusSeconds(120).toEpochMilli()
          + ".u." + "c".repeat(32);
      Map<String, String> entries = new HashMap<>(Map.of(LIVE, "1", expired, "1"));

      int removed = entryFile.removeStaleEntries(entries, NOW);

      assertEquals(0, removed);
      assertEquals(Map.of(LIVE, "1"), entries);
    }

    @Test
    void shouldRemoveTheEarliestExpiryToMakeRoomAtTheLimit() {
      Map<String, String> entries = new HashMap<>();
      for (int index = 0; index < entryFile.getMaxEntries(); index++) {
        entries.put("t2." + NOW.plusSeconds(index + 1L).toEpochMilli() + "." + NOW.toEpochMilli()
            + ".u." + "%032x".formatted(index), "1");
      }

      int removed = entryFile.removeStaleEntries(entries, NOW);

      assertEquals(1, removed);
      assertEquals(9999, entries.size());
      assertFalse(entries.containsKey("t2." + NOW.plusSeconds(1).toEpochMilli() + "."
          + NOW.toEpochMilli() + ".u." + "%032x".formatted(0)));
    }
  }
}
