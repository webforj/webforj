package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getSnapshotCount;
import static com.webforj.devtools.craftforj.history.HistoryFixture.readEntries;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("HistoryJournal skip")
class HistoryJournalSkipTest {

  @TempDir
  Path project;

  @TempDir
  Path store;

  private HistoryJournal journal;
  private Path view;

  @BeforeEach
  void setUp() throws IOException {
    journal = new HistoryJournal(store);
    view = project.resolve("src/main/java/app/View.java");
    Files.createDirectories(view.getParent());
  }

  @Test
  @DisplayName("Should keep a skipped applied entry so the older step of its file still undoes")
  void shouldUndoOlderStepAfterSkip() throws IOException {
    write(view, toBytes("a\n"));
    addWrite(journal, view, toBytes("b\n"));
    long older = journal.getInfo().getEntries().get(0).getId();
    addWrite(journal, view, toBytes("c\n"));
    long skipped = journal.getInfo().getEntries().get(0).getId();
    assertEquals(HistoryRestoreResult.Code.LATER_STEP, journal.undo(older).getCode());

    HistoryJournalInfo info = journal.remove(List.of(skipped));

    assertEquals(List.of(skipped), info.getRemoved());
    assertEquals(List.of(older), info.getEntries().stream().map(HistoryEntryInfo::getId).toList());
    assertTrue(readEntries(store).stream()
        .anyMatch(entry -> entry.isSkipped() && entry.getId() == skipped));
    assertTrue(journal.undo(older).isDone());
    assertArrayEquals(toBytes("a\n"), Files.readAllBytes(view));
    assertTrue(journal.redo(older).isDone());
    assertArrayEquals(toBytes("b\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should pass a skipped entry over and let a new step drop it with the step before")
  void shouldPassSkippedEntryOver() throws IOException {
    journal = new HistoryJournal(store, 1, 10_000);
    write(view, toBytes("a\n"));
    addWrite(journal, view, toBytes("b\n"));
    long skipped = journal.getInfo().getEntries().get(0).getId();
    journal.remove(List.of(skipped));

    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journal.undo().getCode());
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journal.undo(skipped).getCode());
    assertTrue(readEntries(store).isEmpty());

    addWrite(journal, view, toBytes("c\n"));
    addWrite(journal, view, toBytes("d\n"));

    assertEquals(1, readEntries(store).size());
    assertEquals(2, getSnapshotCount(store));
  }

  @Test
  @DisplayName("Should still refuse the step before a skipped entry after a change outside")
  void shouldRefuseAfterSkipAndOutsideChange() throws IOException {
    write(view, toBytes("a\n"));
    addWrite(journal, view, toBytes("b\n"));
    addWrite(journal, view, toBytes("c\n"));
    long skipped = journal.getInfo().getEntries().get(0).getId();
    write(view, toBytes("outside\n"));

    journal.remove(List.of(skipped));

    assertEquals(HistoryRestoreResult.Code.FILE_CHANGED, journal.undo().getCode());
    assertArrayEquals(toBytes("outside\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should remove a skipped undone entry from the journal")
  void shouldRemoveSkippedUndoneEntry() throws IOException {
    write(view, toBytes("a\n"));
    addWrite(journal, view, toBytes("b\n"));
    addWrite(journal, view, toBytes("c\n"));
    long skipped = journal.getInfo().getEntries().get(0).getId();
    assertTrue(journal.undo().isDone());

    HistoryJournalInfo info = journal.remove(List.of(skipped));

    assertEquals(List.of(skipped), info.getRemoved());
    assertEquals(1, info.getEntries().size());
    assertTrue(readEntries(store).stream().noneMatch(entry -> entry.getId() == skipped));
    assertArrayEquals(toBytes("b\n"), Files.readAllBytes(view));
  }
}
