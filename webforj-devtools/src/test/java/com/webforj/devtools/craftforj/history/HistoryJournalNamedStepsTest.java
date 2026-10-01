package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("HistoryJournal named steps")
class HistoryJournalNamedStepsTest {

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
  @DisplayName("Should take the entry the panel names only when it comes next")
  void shouldUndoNamedEntry() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("view one\n"));
    write(other, toBytes("other one\n"));
    addWrite(journal, view, toBytes("view two\n"));
    addWrite(journal, other, toBytes("other two\n"));
    long first = journal.getInfo().getEntries().get(1).getId();
    long second = journal.getInfo().getEntries().get(0).getId();

    HistoryRestoreResult early = journal.undo(first);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, early.getCode());
    assertEquals(List.of(other.toString()), early.getFiles());
    assertArrayEquals(toBytes("view two\n"), Files.readAllBytes(view));
    assertTrue(early.getJournal().getEntries().get(1).isApplied());

    assertTrue(journal.undo(second).isDone());
    assertTrue(journal.undo(first).isDone());
    assertArrayEquals(toBytes("view one\n"), Files.readAllBytes(view));
    assertArrayEquals(toBytes("other one\n"), Files.readAllBytes(other));
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journal.undo(first).getCode());

    assertTrue(journal.redo(first).isDone());
    assertArrayEquals(toBytes("view two\n"), Files.readAllBytes(view));
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, journal.redo(first).getCode());
    assertTrue(journal.redo(second).isDone());
    assertArrayEquals(toBytes("other two\n"), Files.readAllBytes(other));
  }

  @Test
  @DisplayName("Should refuse an undo while a newer entry of another file is applied")
  void shouldRefuseUndoBehindNewerEntryOfOtherFile() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("view one\n"));
    write(other, toBytes("other one\n"));
    addWrite(journal, view, toBytes("view two\n"));
    addWrite(journal, other, toBytes("other two\n"));
    long older = journal.getInfo().getEntries().get(1).getId();

    HistoryRestoreResult result = journal.undo(older);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, result.getCode());
    assertEquals(List.of(other.toString()), result.getFiles());
    assertTrue(journal.getInfo().getEntries().get(1).isApplied());
    assertArrayEquals(toBytes("view two\n"), Files.readAllBytes(view));
    assertArrayEquals(toBytes("other two\n"), Files.readAllBytes(other));
  }

  @Test
  @DisplayName("Should refuse the redo of the newer of two undone entries while the older waits")
  void shouldRefuseRedoOfNewerUndoneEntry() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("view one\n"));
    write(other, toBytes("other one\n"));
    addWrite(journal, view, toBytes("view two\n"));
    addWrite(journal, other, toBytes("other two\n"));
    long newer = journal.getInfo().getEntries().get(0).getId();
    assertTrue(journal.undo(null).isDone());
    assertTrue(journal.undo(null).isDone());

    HistoryRestoreResult result = journal.redo(newer);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, result.getCode());
    assertEquals(List.of(view.toString()), result.getFiles());
    assertArrayEquals(toBytes("view one\n"), Files.readAllBytes(view));
    assertArrayEquals(toBytes("other one\n"), Files.readAllBytes(other));
    assertFalse(result.getJournal().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should redo the oldest undone entry of a journal an earlier build left")
  void shouldRedoOldestUndoneEntryOfEarlierJournal() throws IOException {
    Path other = project.resolve("Other.java");
    Path third = project.resolve("Third.java");
    write(view, toBytes("view one\n"));
    write(other, toBytes("other one\n"));
    write(third, toBytes("third one\n"));
    addWrite(journal, view, toBytes("view two\n"));
    addWrite(journal, other, toBytes("other two\n"));
    addWrite(journal, third, toBytes("third two\n"));
    long middle = journal.getInfo().getEntries().get(1).getId();
    long newest = journal.getInfo().getEntries().get(0).getId();
    Path index = store.resolve("journal.json");
    String[] parts = Files.readString(index).split("\"applied\":true", -1);
    assertEquals(4, parts.length);
    Files.writeString(index, parts[0] + "\"applied\":true" + parts[1]
        + "\"applied\":false,\"undoSequence\":1" + parts[2] + "\"applied\":true" + parts[3]);
    write(other, toBytes("other one\n"));

    assertTrue(journal.undo(newest).isDone());
    HistoryRestoreResult early = journal.redo(newest);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, early.getCode());
    assertEquals(List.of(other.toString()), early.getFiles());
    assertArrayEquals(toBytes("third one\n"), Files.readAllBytes(third));

    assertTrue(journal.redo(middle).isDone());
    assertArrayEquals(toBytes("other two\n"), Files.readAllBytes(other));
    assertTrue(journal.redo(newest).isDone());
    assertArrayEquals(toBytes("third two\n"), Files.readAllBytes(third));
    assertArrayEquals(toBytes("view two\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should refuse an entry the journal does not hold")
  void shouldRefuseUnknownEntry() {
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journal.undo(42L).getCode());
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, journal.redo(42L).getCode());
  }

  @Test
  @DisplayName("Should restore one write of a file written several times, byte for byte")
  void shouldRestoreOneOfSeveralWrites() throws IOException {
    write(view, toBytes("a\n"));
    addWrite(journal, view, toBytes("a\nb\n"));
    addWrite(journal, view, toBytes("a\nb\nc\n"));
    long last = journal.getInfo().getEntries().get(0).getId();
    long before = journal.getInfo().getEntries().get(1).getId();

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, journal.undo(before).getCode());
    assertTrue(journal.undo(last).isDone());
    assertArrayEquals(toBytes("a\nb\n"), Files.readAllBytes(view));
    assertTrue(journal.undo(before).isDone());
    assertArrayEquals(toBytes("a\n"), Files.readAllBytes(view));
    assertTrue(journal.redo(before).isDone());
    assertTrue(journal.redo(last).isDone());
    assertArrayEquals(toBytes("a\nb\nc\n"), Files.readAllBytes(view));
  }
}
