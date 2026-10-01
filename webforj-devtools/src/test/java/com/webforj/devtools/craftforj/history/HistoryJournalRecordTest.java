package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addStep;
import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getPaths;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getSnapshotCount;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import com.webforj.devtools.craftforj.source.staging.SourceHasher;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("HistoryJournal record")
class HistoryJournalRecordTest {

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
  @DisplayName("Should list the entry with the hashes of the file before and after")
  void shouldListEntryWithHashes() {
    write(view, toBytes("class View {}\n"));

    Long id = addStep(journal, List.of(view), () -> {
      write(view, toBytes("class View { int a; }\n"));
      return null;
    });

    HistoryJournalInfo info = journal.getInfo();
    assertEquals(1, info.getEntries().size());
    HistoryEntryInfo entry = info.getEntries().get(0);
    assertEquals(id, entry.getId());
    assertTrue(entry.isApplied());
    HistoryFileSnapshot file = entry.getFiles().get(0);
    assertEquals(view.toString(), file.getPath());
    assertEquals(SourceHasher.hash(toBytes("class View {}\n")), file.getBefore());
    assertEquals(SourceHasher.hash(toBytes("class View { int a; }\n")), file.getAfter());
  }

  @Test
  @DisplayName("Should keep only the files the write changed")
  void shouldKeepOnlyChangedFiles() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("class View {}\n"));
    write(other, toBytes("class Other {}\n"));

    addStep(journal, List.of(view, other), () -> {
      write(view, toBytes("class View { int a; }\n"));
      return null;
    });

    assertEquals(List.of(view.toString()), getPaths(journal.getInfo().getEntries().get(0)));
    assertEquals(2, getSnapshotCount(store));
  }

  @Test
  @DisplayName("Should leave no entry for a write that changed nothing")
  void shouldLeaveNoEntryForUnchangedWrite() {
    write(view, toBytes("class View {}\n"));

    addStep(journal, List.of(view), () -> null);

    assertTrue(journal.getInfo().getEntries().isEmpty());
  }

  @Test
  @DisplayName("Should record what a failing write changed")
  void shouldRecordFailingWrite() {
    write(view, toBytes("class View {}\n"));

    assertThrows(IllegalStateException.class, () -> addStep(journal, List.of(view), () -> {
      write(view, toBytes("class View { int a; }\n"));
      throw new IllegalStateException("boom");
    }));

    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should drop the undone entries with their snapshots when a new step is recorded")
  void shouldDropUndoneEntriesOnNewStep() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    addWrite(journal, view, toBytes("three\n"));
    long undone = journal.getInfo().getEntries().get(0).getId();
    assertTrue(journal.undo(null).isDone());

    addWrite(journal, view, toBytes("four\n"));

    HistoryJournalInfo info = journal.getInfo();
    assertEquals(2, info.getEntries().size());
    assertTrue(info.getEntries().stream().allMatch(HistoryEntryInfo::isApplied));
    assertEquals(undone + 1, info.getEntries().get(0).getId());
    assertEquals(3, getSnapshotCount(store));

    HistoryRestoreResult redo = journal.redo(undone);

    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, redo.getCode());
    assertArrayEquals(toBytes("four\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should name the ids a remove took out and leave the others out")
  void shouldNameRemovedIds() {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    addWrite(journal, view, toBytes("three\n"));
    long newest = journal.getInfo().getEntries().get(0).getId();

    HistoryJournalInfo info = journal.remove(List.of(newest, 999L));

    assertEquals(List.of(newest), info.getRemoved());
    assertEquals(1, info.getEntries().size());
    assertNull(journal.getInfo().getRemoved());
  }

  @Test
  @DisplayName("Should answer the id of the entry when the step closes")
  void shouldAnswerEntryIdOnClose() {
    write(view, toBytes("one\n"));

    Long id = addStep(journal, List.of(view), () -> {
      write(view, toBytes("two\n"));
      return null;
    });

    assertEquals(journal.getInfo().getEntries().get(0).getId(), id);
  }

  @Test
  @DisplayName("Should answer the id even when unused snapshots cannot be removed")
  void shouldAnswerEntryIdWhenCollectionFails() throws IOException {
    Path stale = store.resolve("snapshots").resolve("stale");
    Files.createDirectories(stale);
    Files.writeString(stale.resolve("inside"), "keeps the folder from being removed");
    write(view, toBytes("one\n"));

    Long id = addStep(journal, List.of(view), () -> {
      write(view, toBytes("two\n"));
      return null;
    });

    assertEquals(journal.getInfo().getEntries().get(0).getId(), id);
    assertTrue(Files.isDirectory(stale));
  }

  @Test
  @DisplayName("Should keep the newest entries up to the limit")
  void shouldKeepNewestEntriesUpToLimit() throws IOException {
    journal = new HistoryJournal(store, 2, 10_000);
    write(view, toBytes("0\n"));
    addWrite(journal, view, toBytes("1\n"));
    addWrite(journal, view, toBytes("2\n"));
    addWrite(journal, view, toBytes("3\n"));

    HistoryJournalInfo info = journal.getInfo();
    assertEquals(2, info.getEntries().size());
    assertEquals(3, info.getEntries().get(0).getId());
    assertEquals(2, info.getEntries().get(1).getId());
    assertEquals(3, getSnapshotCount(store));
  }

  @Test
  @DisplayName("Should run the write without an entry when a file cannot be read before it")
  void shouldRunWriteWhenFilesCannotBeRead() throws IOException {
    Path folder = project.resolve("folder");
    Files.createDirectories(folder);
    Path locked = folder.resolve("Locked.java");
    write(locked, toBytes("class Locked {}\n"));
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("-w-------"));
    assumeTrue(!Files.isReadable(locked));

    try {
      Long id = addStep(journal, List.of(locked), () -> null);

      assertNull(id);
      assertTrue(journal.getInfo().getEntries().isEmpty());
    } finally {
      Files.setPosixFilePermissions(locked, PosixFilePermissions.fromString("rw-------"));
    }
  }

  @Test
  @DisplayName("Should keep the write when the journal cannot be written")
  void shouldKeepWriteWhenJournalCannotBeWritten() throws IOException {
    Path blocked = project.resolve("blocked");
    write(blocked, toBytes("a file where the journal directory would be"));
    journal = new HistoryJournal(blocked.resolve("journal"));
    write(view, toBytes("class View {}\n"));

    addStep(journal, List.of(view), () -> {
      write(view, toBytes("class View { int a; }\n"));
      return null;
    });

    assertArrayEquals(toBytes("class View { int a; }\n"), Files.readAllBytes(view));
    assertNull(journal.getInfo());
  }
}
