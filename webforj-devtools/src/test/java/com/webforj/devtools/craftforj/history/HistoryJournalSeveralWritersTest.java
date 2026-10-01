package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addStep;
import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.isLockFree;
import static com.webforj.devtools.craftforj.history.HistoryFixture.toBytes;
import static com.webforj.devtools.craftforj.history.HistoryFixture.write;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("HistoryJournal several writers")
class HistoryJournalSeveralWritersTest {

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
  @DisplayName("Should refuse the redo of a writer whose undone entry another writer replaced")
  void shouldRefuseRedoOfOtherWriter() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    addWrite(journal, view, toBytes("three\n"));
    addWrite(journal, view, toBytes("four\n"));
    HistoryJournal first = new HistoryJournal(store);
    long third = first.getInfo().getEntries().get(0).getId();
    assertTrue(first.undo(third).isDone());
    HistoryJournal second = new HistoryJournal(store);
    addStep(second, List.of(view), () -> {
      write(view, toBytes("five\n"));
      return null;
    });

    HistoryRestoreResult result = first.redo(third);

    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, result.getCode());
    assertEquals(List.of(third + 1, third - 1, third - 2),
        result.getJournal().getEntries().stream().map(HistoryEntryInfo::getId).toList());
    assertArrayEquals(toBytes("five\n"), Files.readAllBytes(view));
  }

  @Test
  @DisplayName("Should take turns between two journals of one directory on one file")
  void shouldTakeTurnsOnOneFile() throws Exception {
    HistoryJournal first = new HistoryJournal(store);
    HistoryJournal second = new HistoryJournal(store);
    write(view, toBytes("start\n"));
    List<String> written = Collections.synchronizedList(new ArrayList<>());
    List<Thread> threads = new ArrayList<>();
    for (int writer = 0; writer < 2; writer++) {
      HistoryJournal target = writer == 0 ? first : second;
      String name = "writer " + writer;
      threads.add(new Thread(() -> {
        for (int step = 0; step < 50; step++) {
          String content = name + " step " + step + "\n";
          addStep(target, List.of(view), () -> {
            write(view, toBytes(content));
            written.add(content);
            return null;
          });
        }
      }));
    }

    threads.forEach(Thread::start);
    for (Thread thread : threads) {
      thread.join();
    }

    List<Long> ids = first.getInfo().getEntries().stream().map(HistoryEntryInfo::getId).toList();
    assertEquals(100, ids.size());
    assertEquals(100, ids.stream().distinct().count());
    assertEquals(100, written.size());
    for (int position = written.size() - 1; position >= 0; position--) {
      HistoryJournal target = position % 2 == 0 ? first : second;

      assertTrue(target.undo().isDone());
      assertArrayEquals(toBytes(position == 0 ? "start\n" : written.get(position - 1)),
          Files.readAllBytes(view));
    }
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, second.undo().getCode());
    assertTrue(isLockFree(store));
    assertNotNull(new HistoryJournal(store, 100, 50).getInfo());
  }

  @Test
  @DisplayName("Should take turns between two journals of one directory on two files")
  void shouldTakeTurnsOnTwoFiles() throws Exception {
    List<HistoryJournal> journals = List.of(new HistoryJournal(store), new HistoryJournal(store));
    List<Path> files = List.of(project.resolve("First.java"), project.resolve("Second.java"));
    List<List<String>> written = List.of(new ArrayList<>(), new ArrayList<>());
    List<Thread> threads = new ArrayList<>();
    for (int writer = 0; writer < 2; writer++) {
      int index = writer;
      write(files.get(index), toBytes("start " + index + "\n"));
      threads.add(new Thread(() -> {
        for (int step = 0; step < 50; step++) {
          String content = "writer " + index + " step " + step + "\n";
          addStep(journals.get(index), List.of(files.get(index)), () -> {
            write(files.get(index), toBytes(content));
            return null;
          });
          written.get(index).add(content);
        }
      }));
    }

    threads.forEach(Thread::start);
    for (Thread thread : threads) {
      thread.join();
    }

    List<HistoryEntryInfo> entries = journals.get(0).getInfo().getEntries();
    assertEquals(100, entries.size());
    assertEquals(100, entries.stream().map(HistoryEntryInfo::getId).distinct().count());
    int[] left = {50, 50};
    for (int step = 0; step < 100; step++) {
      HistoryRestoreResult result = journals.get(step % 2).undo();
      int index = files.get(0).toString().equals(result.getFiles().get(0)) ? 0 : 1;
      left[index]--;

      assertTrue(result.isDone());
      assertArrayEquals(
          toBytes(
              left[index] == 0 ? "start " + index + "\n" : written.get(index).get(left[index] - 1)),
          Files.readAllBytes(files.get(index)));
    }
    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, journals.get(1).undo().getCode());
    assertTrue(isLockFree(store));
    assertNotNull(new HistoryJournal(store, 100, 50).getInfo());
  }

  @Test
  @DisplayName("Should hold an undo of one journal until the write another journal records ends")
  void shouldNeverInterleaveUndoWithRecord() throws Exception {
    HistoryJournal recording = new HistoryJournal(store);
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    CountDownLatch holding = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    Thread holder = new Thread(() -> addStep(recording, List.of(view), () -> {
      write(view, toBytes("three\n"));
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

    HistoryJournal undoing = new HistoryJournal(store);
    CompletableFuture<HistoryRestoreResult> undo =
        CompletableFuture.supplyAsync(() -> undoing.undo());

    assertThrows(TimeoutException.class, () -> undo.get(300, TimeUnit.MILLISECONDS));
    assertArrayEquals(toBytes("three\n"), Files.readAllBytes(view));

    release.countDown();
    holder.join();
    HistoryRestoreResult result = undo.get();

    assertTrue(result.isDone());
    assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    assertEquals(2, recording.getInfo().getEntries().size());
    assertFalse(recording.getInfo().getEntries().get(0).isApplied());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should keep every entry two journals on two threads record in increasing order")
  void shouldKeepEveryEntryInOrder() throws Exception {
    List<HistoryJournal> journals = List.of(new HistoryJournal(store), new HistoryJournal(store));
    List<List<Long>> recorded = List.of(Collections.synchronizedList(new ArrayList<>()),
        Collections.synchronizedList(new ArrayList<>()));
    List<Thread> threads = new ArrayList<>();
    for (int writer = 0; writer < 2; writer++) {
      int index = writer;
      Path file = project.resolve("Writer" + index + ".java");
      threads.add(new Thread(() -> {
        for (int step = 0; step < 40; step++) {
          byte[] content = toBytes("writer " + index + " step " + step + "\n");
          recorded.get(index).add(addStep(journals.get(index), List.of(file), () -> {
            write(file, content);
            return null;
          }));
        }
      }));
    }

    threads.forEach(Thread::start);
    for (Thread thread : threads) {
      thread.join();
    }

    HistoryJournalInfo info = journals.get(0).getInfo();
    List<Long> ids = info.getEntries().stream().map(HistoryEntryInfo::getId).toList();
    assertEquals(80, ids.size());
    assertEquals(80L, ids.get(0));
    for (int position = 1; position < ids.size(); position++) {
      assertTrue(ids.get(position - 1) > ids.get(position));
    }
    for (List<Long> own : recorded) {
      assertEquals(40, own.size());
      for (int position = 1; position < own.size(); position++) {
        assertTrue(own.get(position - 1) < own.get(position));
      }
    }
    List<Long> all = new ArrayList<>(recorded.get(0));
    all.addAll(recorded.get(1));
    Collections.sort(all);
    assertEquals(ids.reversed(), all);
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should give every entry of two journals on one directory its own id")
  void shouldKeepIdsUniqueAcrossInstances() throws Exception {
    HistoryJournal other = new HistoryJournal(store);
    List<Thread> threads = new ArrayList<>();
    for (int writer = 0; writer < 4; writer++) {
      HistoryJournal target = writer % 2 == 0 ? journal : other;
      Path file = project.resolve("File" + writer + ".java");
      threads.add(new Thread(() -> {
        for (int step = 0; step < 10; step++) {
          byte[] content = toBytes("step " + step + "\n");
          addStep(target, List.of(file), () -> {
            write(file, content);
            return null;
          });
        }
      }));
    }

    threads.forEach(Thread::start);
    for (Thread thread : threads) {
      thread.join();
    }

    List<Long> ids = journal.getInfo().getEntries().stream().map(HistoryEntryInfo::getId).toList();
    assertEquals(40, ids.size());
    assertEquals(40, ids.stream().distinct().count());
  }

  @Test
  @DisplayName("Should never hand out an id again after its entry was dropped")
  void shouldNeverReuseIds() {
    HistoryJournal small = new HistoryJournal(store, 1, 10_000);
    write(view, toBytes("one\n"));
    addStep(small, List.of(view), () -> {
      write(view, toBytes("two\n"));
      return null;
    });
    long first = small.getInfo().getEntries().get(0).getId();
    small.remove(List.of(first));
    addStep(small, List.of(view), () -> {
      write(view, toBytes("three\n"));
      return null;
    });

    assertEquals(first + 1, small.getInfo().getEntries().get(0).getId());
  }

  @Test
  @DisplayName("Should refuse a step as busy while another writer holds the journal")
  void shouldRefuseWhileHeld() throws Exception {
    HistoryJournal impatient = new HistoryJournal(store, 100, 50);
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    CountDownLatch holding = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    Thread holder = new Thread(() -> addStep(journal, List.of(view), () -> {
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

    try {
      HistoryRestoreResult busy = impatient.undo();

      assertEquals(HistoryRestoreResult.Code.BUSY, busy.getCode());
      assertNull(busy.getJournal());
      assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    } finally {
      release.countDown();
      holder.join();
    }

    assertTrue(impatient.undo().isDone());
    assertTrue(isLockFree(store));
  }
}
