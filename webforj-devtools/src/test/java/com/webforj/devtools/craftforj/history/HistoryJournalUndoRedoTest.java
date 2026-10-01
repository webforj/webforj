package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addStep;
import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

@DisplayName("HistoryJournal undo and redo")
class HistoryJournalUndoRedoTest {

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
  @DisplayName("Should restore the file byte for byte on undo and reapply it on redo")
  void shouldRestoreByteForByte() throws IOException {
    byte[] before = toBytes("﻿package app;\r\n\r\nclass View { String s = \"größe ✓\"; }\r\n");
    byte[] after = toBytes("﻿package app;\r\n\r\nclass View { String s = \"size\"; }\r\n");
    write(view, before);
    addWrite(journal, view, after);

    HistoryRestoreResult undone = journal.undo(null);

    assertTrue(undone.isDone());
    assertNull(undone.getCode());
    assertArrayEquals(before, Files.readAllBytes(view));
    assertEquals(List.of(view.toString()), undone.getFiles());
    assertFalse(undone.getEntry().isApplied());
    assertFalse(undone.getJournal().getEntries().get(0).isApplied());

    HistoryRestoreResult redone = journal.redo(null);

    assertTrue(redone.isDone());
    assertArrayEquals(after, Files.readAllBytes(view));
    assertTrue(redone.getEntry().isApplied());
    assertTrue(redone.getJournal().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should walk several entries back and forth in order")
  void shouldWalkSeveralEntries() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    addWrite(journal, view, toBytes("three\n"));

    journal.undo(null);
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    journal.undo(null);
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(view));
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journal.undo(null).getCode());
    journal.redo(null);
    journal.redo(null);
    assertArrayEquals(toBytes("three\n"), Files.readAllBytes(view));
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, journal.redo(null).getCode());
  }

  @Test
  @DisplayName("Should delete a created file on undo and create it again on redo")
  void shouldDeleteCreatedFileOnUndo() throws IOException {
    Path created = project.resolve("src/main/java/app/Created.java");

    addStep(journal, List.of(created), () -> {
      try {
        Files.createDirectories(created.getParent());
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
      write(created, toBytes("class Created {}\n"));
      return null;
    });

    assertTrue(journal.undo(null).isDone());
    assertFalse(Files.exists(created));
    assertTrue(journal.redo(null).isDone());
    assertArrayEquals(toBytes("class Created {}\n"), Files.readAllBytes(created));
  }

  @Test
  @DisplayName("Should put a deleted file back on undo")
  void shouldPutDeletedFileBackOnUndo() throws IOException {
    write(view, toBytes("class View {}\n"));

    addStep(journal, List.of(view), () -> {
      try {
        Files.delete(view);
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }
      return null;
    });

    assertTrue(journal.undo(null).isDone());
    assertArrayEquals(toBytes("class View {}\n"), Files.readAllBytes(view));
    assertTrue(journal.redo(null).isDone());
    assertFalse(Files.exists(view));
  }

  @Test
  @DisplayName("Should keep the permissions of every file through an undo and a redo")
  void shouldKeepPermissions() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path script = project.resolve("run.sh");
    write(view, toBytes("one\n"));
    write(script, toBytes("echo one\n"));
    Files.setPosixFilePermissions(view, PosixFilePermissions.fromString("rw-r--r--"));
    Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"));
    addStep(journal, List.of(view, script), () -> {
      write(view, toBytes("two\n"));
      write(script, toBytes("echo two\n"));
      return null;
    });

    assertTrue(journal.undo(null).isDone());
    assertEquals("rw-r--r--", PosixFilePermissions.toString(Files.getPosixFilePermissions(view)));
    assertEquals("rwxr-xr-x", PosixFilePermissions.toString(Files.getPosixFilePermissions(script)));
    assertTrue(journal.redo(null).isDone());
    assertEquals("rw-r--r--", PosixFilePermissions.toString(Files.getPosixFilePermissions(view)));
    assertEquals("rwxr-xr-x", PosixFilePermissions.toString(Files.getPosixFilePermissions(script)));
    assertArrayEquals(toBytes("echo two\n"), Files.readAllBytes(script));
  }

  @Test
  @DisplayName("Should keep a file that is a symbolic link a link through an undo and a redo")
  void shouldKeepSymbolicLink() throws IOException {
    Path real = project.resolve("real/View.java");
    Files.createDirectories(real.getParent());
    write(real, toBytes("one\n"));
    Path link = project.resolve("Linked.java");
    try {
      Files.createSymbolicLink(link, real);
    } catch (IOException | UnsupportedOperationException e) {
      assumeTrue(false, "The file system cannot create links");
    }
    addWrite(journal, link, toBytes("two\n"));

    assertTrue(journal.undo(null).isDone());
    assertTrue(Files.isSymbolicLink(link));
    assertEquals(real, Files.readSymbolicLink(link));
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(real));
    assertTrue(journal.redo(null).isDone());
    assertTrue(Files.isSymbolicLink(link));
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(real));
    try (Stream<Path> files = Files.list(real.getParent())) {
      assertEquals(List.of(real), files.toList());
    }
  }

  @Test
  @DisplayName("Should give a file a redo creates again the default permissions")
  void shouldCreateFileWithDefaultPermissions() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path probe = project.resolve("Probe.java");
    write(probe, toBytes("probe\n"));
    Set<PosixFilePermission> defaults = Files.getPosixFilePermissions(probe);
    assumeTrue(!defaults.equals(PosixFilePermissions.fromString("rw-------")));
    Path created = project.resolve("Created.java");
    addStep(journal, List.of(created), () -> {
      write(created, toBytes("class Created {}\n"));
      return null;
    });

    assertTrue(journal.undo(null).isDone());
    assertFalse(Files.exists(created));
    assertTrue(journal.redo(null).isDone());
    assertArrayEquals(toBytes("class Created {}\n"), Files.readAllBytes(created));
    assertEquals(defaults, Files.getPosixFilePermissions(created));
  }

  @Test
  @DisplayName("Should remove a file a redo created when the journal cannot be written after it")
  void shouldRemoveCreatedFileWhenJournalWriteFails() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path created = project.resolve("Created.java");
    addStep(journal, List.of(created), () -> {
      write(created, toBytes("class Created {}\n"));
      return null;
    });
    assertTrue(journal.undo(null).isDone());
    Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("r-x------"));

    HistoryRestoreResult result;
    try {
      result = journal.redo(null);
    } finally {
      Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("rwx------"));
    }

    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, result.getCode());
    assertFalse(Files.exists(created));
  }

  @Test
  @DisplayName("Should refuse an undo behind a later applied step after an outside revert")
  void shouldRefuseUndoBehindLaterAppliedStep() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    long first = journal.getInfo().getEntries().get(0).getId();
    addWrite(journal, view, toBytes("three\n"));
    write(view, toBytes("two\n"));

    HistoryRestoreResult result = journal.undo(first);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, result.getCode());
    assertEquals(List.of(view.toString()), result.getFiles());
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    assertTrue(journal.getInfo().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should refuse the redo of an entry a later step replaced")
  void shouldRefuseRedoReplacedByLaterStep() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    long first = journal.getInfo().getEntries().get(0).getId();
    assertTrue(journal.undo(first).isDone());
    addWrite(journal, view, toBytes("three\n"));

    HistoryRestoreResult result = journal.redo(first);

    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, result.getCode());
    assertArrayEquals(toBytes("three\n"), Files.readAllBytes(view));
    assertEquals(1, result.getJournal().getEntries().size());
    assertTrue(result.getJournal().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should name a later step before a change made outside and list both apart")
  void shouldListBlockedAndChangedFilesApart() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("one\n"));
    write(other, toBytes("other one\n"));
    addStep(journal, List.of(view, other), () -> {
      write(view, toBytes("two\n"));
      write(other, toBytes("other two\n"));
      return null;
    });
    long first = journal.getInfo().getEntries().get(0).getId();
    addWrite(journal, view, toBytes("three\n"));
    write(other, toBytes("outside\n"));

    HistoryRestoreResult result = journal.undo(first);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, result.getCode());
    assertEquals(List.of(view.toString()), result.getFiles());
    assertArrayEquals(toBytes("three\n"), Files.readAllBytes(view));
    assertArrayEquals(toBytes("outside\n"), Files.readAllBytes(other));
  }

  @Test
  @DisplayName("Should refuse an undo behind a later applied step of several files sharing one")
  void shouldRefuseUndoBehindLaterStepOfSeveralFiles() throws IOException {
    Path other = project.resolve("Other.java");
    write(view, toBytes("one\n"));
    write(other, toBytes("other one\n"));
    addWrite(journal, view, toBytes("two\n"));
    long first = journal.getInfo().getEntries().get(0).getId();
    addStep(journal, List.of(other, view), () -> {
      write(other, toBytes("other two\n"));
      write(view, toBytes("three\n"));
      return null;
    });
    write(view, toBytes("two\n"));

    HistoryRestoreResult result = journal.undo(first);

    assertEquals(HistoryRestoreResult.Code.LATER_STEP, result.getCode());
    assertEquals(List.of(other.toString(), view.toString()), result.getFiles());
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    assertArrayEquals(toBytes("other two\n"), Files.readAllBytes(other));
    assertTrue(journal.getInfo().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should restore every file when writing one throws after another was moved")
  void shouldRestoreFilesWhenWriteThrows() throws IOException {
    Path created = project.resolve("Created.java");
    write(view, toBytes("one\n"));
    addStep(journal, List.of(view, created), () -> {
      write(view, toBytes("two\n"));
      write(created, toBytes("class Created {}\n"));
      return null;
    });
    assertTrue(journal.undo(null).isDone());
    Path refused = created.toAbsolutePath().normalize();

    HistoryRestoreResult result;
    try (
        MockedStatic<ProjectFileWriter> writer = mockStatic(ProjectFileWriter.class, invocation -> {
          if ("reset".equals(invocation.getMethod().getName())
              && refused.equals(invocation.getArgument(0))) {
            throw new SecurityException("Writing " + refused + " is not allowed");
          }

          return invocation.callRealMethod();
        })) {
      result = journal.redo(null);
    }

    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, result.getCode());
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(view));
    assertFalse(Files.exists(created));
    assertFalse(journal.getInfo().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should restore a file an undo deleted with the permissions it had")
  void shouldRestoreDeletedFileWithItsPermissions() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path created = project.resolve("New.java");
    write(view, toBytes("one\n"));
    addStep(journal, List.of(created, view), () -> {
      write(created, toBytes("class New {}\n"));
      write(view, toBytes("two\n"));
      return null;
    });
    Files.setPosixFilePermissions(created, PosixFilePermissions.fromString("rwxr-x---"));
    Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("r-x------"));

    HistoryRestoreResult result;
    try {
      result = journal.undo(null);
    } finally {
      Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("rwx------"));
    }

    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, result.getCode());
    assertArrayEquals(toBytes("class New {}\n"), Files.readAllBytes(created));
    assertEquals("rwxr-x---",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(created)));
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should refuse an undo when the file changed outside craftforJ")
  void shouldRefuseUndoAfterOutsideChange() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    write(view, toBytes("edited in the IDE\n"));

    HistoryRestoreResult result = journal.undo(null);

    assertFalse(result.isDone());
    assertEquals(HistoryRestoreResult.Code.FILE_CHANGED, result.getCode());
    assertEquals(List.of(view.toString()), result.getFiles());
    assertEquals("Changed outside craftforJ since the step was recorded, " + view,
        result.getMessage());
    assertArrayEquals(toBytes("edited in the IDE\n"), Files.readAllBytes(view));
    assertTrue(result.getJournal().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should refuse a redo when the file changed outside craftforJ")
  void shouldRefuseRedoAfterOutsideChange() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    journal.undo(null);
    write(view, toBytes("edited in the IDE\n"));

    HistoryRestoreResult result = journal.redo(null);

    assertEquals(HistoryRestoreResult.Code.FILE_CHANGED, result.getCode());
    assertEquals(List.of(view.toString()), result.getFiles());
    assertArrayEquals(toBytes("edited in the IDE\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should refuse a step whose snapshot is gone")
  void shouldRefuseStepWithMissingSnapshot() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    try (Stream<Path> snapshots = Files.list(store.resolve("snapshots"))) {
      for (Path snapshot : snapshots.toList()) {
        Files.delete(snapshot);
      }
    }

    HistoryRestoreResult result = journal.undo(null);

    assertEquals(HistoryRestoreResult.Code.SNAPSHOT_MISSING, result.getCode());
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should restore every file when a write fails")
  void shouldRestoreFilesWhenWriteFails() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path folder = project.resolve("locked");
    Files.createDirectories(folder);
    Path created = folder.resolve("Created.java");
    write(view, toBytes("one\n"));

    addStep(journal, List.of(view, created), () -> {
      write(view, toBytes("two\n"));
      write(created, toBytes("class Created {}\n"));
      return null;
    });
    Files.setPosixFilePermissions(folder, PosixFilePermissions.fromString("r-x------"));

    try {
      HistoryRestoreResult result = journal.undo(null);

      assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, result.getCode());
      assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
      assertArrayEquals(toBytes("class Created {}\n"), Files.readAllBytes(created));
      assertTrue(result.getJournal().getEntries().get(0).isApplied());
    } finally {
      Files.setPosixFilePermissions(folder, PosixFilePermissions.fromString("rwx------"));
    }
  }

  @Test
  @DisplayName("Should restore the files and say so when the journal cannot be written after "
      + "them, leaving no staged copy behind")
  void shouldRestoreFilesWhenJournalWriteFails() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("r-x------"));

    HistoryRestoreResult result;
    try {
      result = journal.undo(null);
    } finally {
      Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("rwx------"));
    }

    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, result.getCode());
    assertEquals("The step could not be completed, every file holds what it held before",
        result.getMessage());
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    assertTrue(result.getJournal().getEntries().get(0).isApplied());
    try (Stream<Path> files = Files.list(view.getParent())) {
      assertEquals(List.of(view), files.toList());
    }
  }

  @Test
  @DisplayName("Should name the file it could not restore when the journal write fails after it")
  void shouldReportFilesNotRestored() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path created = project.resolve("V.java");
    addStep(journal, List.of(created), () -> {
      write(created, toBytes("class V {}\n"));
      return null;
    });
    Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("r-x------"));

    HistoryRestoreResult result;
    try (
        MockedStatic<ProjectFileWriter> writer = mockStatic(ProjectFileWriter.class, invocation -> {
          if ("reset".equals(invocation.getMethod().getName())
              && invocation.getArgument(1) != null) {
            throw new IOException("The disk refuses " + invocation.getArgument(0));
          }

          return invocation.callRealMethod();
        })) {
      result = journal.undo(null);
    } finally {
      Files.setPosixFilePermissions(store, PosixFilePermissions.fromString("rwx------"));
    }

    assertEquals(HistoryRestoreResult.Code.RESTORE_INCOMPLETE, result.getCode());
    assertEquals("The step could not be completed and these files could not be put back: "
        + created.toAbsolutePath().normalize(), result.getMessage());
    assertEquals(List.of(created.toAbsolutePath().normalize().toString()), result.getFiles());
    assertTrue(result.getJournal().getEntries().get(0).isApplied());
  }
}
