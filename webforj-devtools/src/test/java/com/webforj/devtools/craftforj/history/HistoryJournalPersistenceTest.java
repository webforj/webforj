package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addStep;
import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getPaths;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getSnapshotCount;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;

import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;

@DisplayName("HistoryJournal persistence")
class HistoryJournalPersistenceTest {

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
  @DisplayName("Should hand the history to a new journal on the same directory")
  void shouldSurviveNewInstance() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));

    HistoryJournal reloaded = new HistoryJournal(store);

    assertEquals(1, reloaded.getInfo().getEntries().size());
    assertTrue(reloaded.undo().isDone());
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(view));
    assertEquals(1, journal.getInfo().getEntries().size());
    assertFalse(journal.getInfo().getEntries().get(0).isApplied());
  }

  @Test
  @DisplayName("Should keep one identity through entries, undo, redo, remove and a reload")
  void shouldKeepJournalId() throws IOException {
    assertNull(journal.getInfo().getJournalId());
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    String id = journal.getInfo().getJournalId();

    assertNotNull(id);
    addWrite(journal, view, toBytes("three\n"));
    assertEquals(id, journal.getInfo().getJournalId());
    assertEquals(id, journal.undo().getJournal().getJournalId());
    assertEquals(id, journal.redo().getJournal().getJournalId());
    long newest = journal.getInfo().getEntries().get(0).getId();
    assertEquals(id, journal.remove(List.of(newest)).getJournalId());
    assertEquals(id, new HistoryJournal(store).getInfo().getJournalId());
    assertTrue(Files.readString(store.resolve("journal.json")).contains(id));
  }

  @Test
  @DisplayName("Should answer a new identity once the journal folder was removed")
  void shouldChangeJournalIdAfterFolderRemoved() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    final String before = journal.getInfo().getJournalId();
    try (Stream<Path> files = Files.walk(store)) {
      for (Path file : files.sorted(Comparator.reverseOrder()).toList()) {
        Files.delete(file);
      }
    }

    HistoryJournal fresh = new HistoryJournal(store);
    assertNull(fresh.getInfo().getJournalId());
    addStep(fresh, List.of(view), () -> {
      write(view, toBytes("three\n"));
      return null;
    });

    assertNotNull(fresh.getInfo().getJournalId());
    assertNotEquals(before, fresh.getInfo().getJournalId());
    assertEquals(1, fresh.getInfo().getEntries().get(0).getId());
  }

  @Test
  @DisplayName("Should give a journal written before identities one at its next write")
  void shouldGiveOldJournalAnId() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Path index = store.resolve("journal.json");
    Files.writeString(index, Files.readString(index).replaceAll("\"journalId\":\"[^\"]*\",?", ""));

    assertNull(journal.getInfo().getJournalId());
    assertEquals(1, journal.getInfo().getEntries().size());

    String id = journal.undo().getJournal().getJournalId();

    assertNotNull(id);
    assertEquals(id, journal.getInfo().getJournalId());
  }

  @Test
  @DisplayName("Should start empty when the journal file holds no valid journal")
  void shouldStartEmptyWhenJournalIsCorrupt() throws IOException {
    Files.createDirectories(store);
    Files.writeString(store.resolve("journal.json"), "{ not json");

    assertTrue(journal.getInfo().getEntries().isEmpty());

    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));

    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should set an invalid journal aside and leave the snapshots it named")
  void shouldSetCorruptJournalAside() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Files.writeString(store.resolve("journal.json"), "{ not json");

    addWrite(journal, view, toBytes("three\n"));

    List<Path> aside;
    try (Stream<Path> files = Files.list(store)) {
      aside =
          files.filter(file -> file.getFileName().toString().startsWith("journal.json.corrupt-"))
              .toList();
    }
    assertEquals(1, aside.size());
    assertEquals("{ not json", Files.readString(aside.get(0)));
    assertEquals(3, getSnapshotCount(store));
    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should keep counting ids after the journal was set aside or deleted")
  void shouldKeepCountingIdsAfterLostJournal() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    addWrite(journal, view, toBytes("three\n"));
    long newest = journal.getInfo().getEntries().get(0).getId();
    Files.writeString(store.resolve("journal.json"), "{ not json");

    addWrite(journal, view, toBytes("four\n"));

    assertEquals(newest + 1, journal.getInfo().getEntries().get(0).getId());

    Files.delete(store.resolve("journal.json"));
    addWrite(journal, view, toBytes("five\n"));

    assertEquals(newest + 2, journal.getInfo().getEntries().get(0).getId());
  }

  @Test
  @DisplayName("Should skip entries the journal cannot use and keep the rest")
  void shouldSkipOddEntries() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Path index = store.resolve("journal.json");
    Files.writeString(index, Files.readString(index).replace("\"entries\":[",
        "\"entries\":[null,{\"id\":7,\"applied\":true},"));

    HistoryJournalInfo info = journal.getInfo();

    assertEquals(1, info.getEntries().size());
    assertEquals(List.of(view.toString()), getPaths(info.getEntries().get(0)));
    assertTrue(journal.undo().isDone());
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should answer no journal, never an empty one, when its directory cannot be used")
  void shouldAnswerNoJournalWhenDirectoryIsUnusable() throws IOException {
    Path blocked = project.resolve("blocked");
    Files.writeString(blocked, "not a directory");
    HistoryJournal unusable = new HistoryJournal(blocked);

    HistoryRestoreResult undo = unusable.undo();

    assertNull(unusable.getInfo());
    assertNull(unusable.remove(List.of(1L)));
    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, undo.getCode());
    assertNull(undo.getJournal());
  }

  @Test
  @DisplayName("Should answer no journal when the journal file cannot be read")
  void shouldAnswerNoJournalWhenJournalIsUnreadable() throws IOException {
    Files.createDirectories(store.resolve("journal.json"));

    HistoryRestoreResult undo = journal.undo();

    assertNull(journal.getInfo());
    assertEquals(HistoryRestoreResult.Code.RESTORE_FAILED, undo.getCode());
    assertNull(undo.getJournal());
  }

  @Test
  @DisplayName("Should remove what a crash left behind and nothing else")
  void shouldCleanupTemporaryFiles() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Path staged = view.resolveSibling(".View.java4711.craftforj");
    Path hidden = view.resolveSibling(".gitignore");
    Path index = store.resolve("journal.json4711.tmp");
    Path snapshot = store.resolve("snapshots").resolve("0a1b.tmp");
    for (Path temporary : List.of(staged, hidden, index, snapshot)) {
      write(temporary, toBytes("left\n"));
    }

    journal.clearTemporaryFiles();

    assertFalse(Files.exists(staged));
    assertFalse(Files.exists(index));
    assertFalse(Files.exists(snapshot));
    assertTrue(Files.exists(hidden));
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should remove a staged copy left beside the real file of a link")
  void shouldCleanupTemporaryFilesBesideLinkedFile() throws IOException {
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
    Path staged = real.resolveSibling(".View.java4711.craftforj");
    write(staged, toBytes("left\n"));

    journal.clearTemporaryFiles();

    assertFalse(Files.exists(staged));
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(real));
  }

  @Test
  @DisplayName("Should keep the snapshots of a journal set aside at start through later writes")
  void shouldKeepSnapshotsOfJournalSetAsideAtCreate() throws IOException {
    Path directory = HistoryJournal.resolveDirectory(store, project,
        HistoryJournalPersistenceTest.class.getName());
    HistoryJournal created =
        HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);
    write(view, toBytes("one\n"));
    addStep(created, List.of(view), () -> {
      write(view, toBytes("two\n"));
      return null;
    });
    List<Path> named;
    try (Stream<Path> snapshots = Files.list(directory.resolve("snapshots"))) {
      named = snapshots.toList();
    }
    Files.writeString(directory.resolve("journal.json"), "{ not json");
    Path other = project.resolve("Other.java");
    write(other, toBytes("other one\n"));

    HistoryJournal reopened =
        HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);
    addStep(reopened, List.of(other), () -> {
      write(other, toBytes("other two\n"));
      return null;
    });

    assertEquals(2, named.size());
    for (Path snapshot : named) {
      assertTrue(Files.exists(snapshot));
    }
    assertEquals(1, reopened.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should remove what a crash left behind on the first create of a directory only")
  void shouldCleanupOnFirstCreateOnly() throws IOException {
    Path directory = HistoryJournal.resolveDirectory(store, project,
        HistoryJournalPersistenceTest.class.getName());
    Files.createDirectories(directory);
    Path first = directory.resolve("journal.json4711.tmp");
    write(first, toBytes("left\n"));

    HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);

    assertFalse(Files.exists(first));

    Path second = directory.resolve("journal.json4712.tmp");
    write(second, toBytes("left\n"));

    HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);

    assertTrue(Files.exists(second));
  }

  @Test
  @DisplayName("Should return at once from a create while the journal is held and clean up later")
  void shouldCreateAtOnceWhileHeld() throws Exception {
    Path directory = HistoryJournal.resolveDirectory(store, project,
        HistoryJournalPersistenceTest.class.getName());
    Files.createDirectories(directory);
    Path left = directory.resolve("journal.json4711.tmp");
    write(left, toBytes("left\n"));
    write(view, toBytes("one\n"));
    CountDownLatch holding = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    Thread holder = new Thread(() -> addStep(new HistoryJournal(directory), List.of(view), () -> {
      holding.countDown();
      try {
        release.await();
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
      return null;
    }));
    holder.start();
    holding.await();

    long elapsed;
    try {
      long start = System.nanoTime();
      HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);
      elapsed = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);

      assertTrue(Files.exists(left));
    } finally {
      release.countDown();
      holder.join();
    }

    HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);

    assertTrue(elapsed < 1_000, "create waited " + elapsed + " ms");
    assertFalse(Files.exists(left));
  }

  @Test
  @DisplayName("Should start empty when the journal file is empty")
  void shouldStartEmptyWhenJournalIsEmpty() throws IOException {
    Files.createDirectories(store);
    Files.writeString(store.resolve("journal.json"), "");

    assertTrue(journal.getInfo().getEntries().isEmpty());
  }

  @Test
  @DisplayName("Should read no project file for a listing and a file twice for an undo")
  void shouldReadFileOncePerCall() throws IOException {
    write(view, toBytes("0\n"));
    for (int step = 1; step <= 20; step++) {
      addWrite(journal, view, toBytes(step + "\n"));
    }
    for (int step = 0; step < 10; step++) {
      assertTrue(journal.undo().isDone());
    }

    HistoryJournalInfo info;
    HistoryRestoreResult undo;
    try (MockedStatic<Files> files = mockStatic(Files.class, CALLS_REAL_METHODS)) {
      info = journal.getInfo();

      files.verify(() -> Files.readAllBytes(view), times(0));
      files.clearInvocations();

      undo = journal.undo();

      files.verify(() -> Files.readAllBytes(view), times(2));
    }

    assertEquals(20, info.getEntries().size());
    assertTrue(undo.isDone());
    assertArrayEquals(toBytes("9\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should resolve one directory per project and application")
  void shouldResolveDirectoryPerProjectAndApplication() {
    Path home = Path.of("/home/dev");
    Path first = HistoryJournal.resolveDirectory(home, Path.of("/work/shop"), "app.Shop");

    assertEquals(first, HistoryJournal.resolveDirectory(home, Path.of("/work/shop/"), "app.Shop"));
    assertNotEquals(first,
        HistoryJournal.resolveDirectory(home, Path.of("/work/shop"), "app.Admin"));
    assertNotEquals(first, HistoryJournal.resolveDirectory(home, Path.of("/work/crm"), "app.Shop"));
    assertEquals(home.resolve(".webforj/devtools/history"), first.getParent());
    assertEquals(16, first.getFileName().toString().length());
  }

  @Test
  @DisplayName("Should create the journal of an application under the home it is given")
  void shouldCreateJournalUnderHome() {
    HistoryJournal created =
        HistoryJournal.create(store, project, HistoryJournalPersistenceTest.class);
    write(view, toBytes("one\n"));
    addStep(created, List.of(view), () -> {
      write(view, toBytes("two\n"));
      return null;
    });

    assertTrue(Files.isRegularFile(HistoryJournal
        .resolveDirectory(store, project, HistoryJournalPersistenceTest.class.getName())
        .resolve("journal.json")));
  }
}
