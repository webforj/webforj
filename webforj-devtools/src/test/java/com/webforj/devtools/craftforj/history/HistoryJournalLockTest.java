package com.webforj.devtools.craftforj.history;

import static com.webforj.devtools.craftforj.history.HistoryFixture.addStep;
import static com.webforj.devtools.craftforj.history.HistoryFixture.addWrite;
import static com.webforj.devtools.craftforj.history.HistoryFixture.getLockFile;
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

import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.ProcessBuilder.Redirect;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("HistoryJournal lock")
class HistoryJournalLockTest {

  @TempDir
  Path project;

  @TempDir
  Path store;

  @TempDir
  Path programs;

  private HistoryJournal journal;
  private Path view;

  @BeforeEach
  void setUp() throws IOException {
    journal = new HistoryJournal(store);
    view = project.resolve("src/main/java/app/View.java");
    Files.createDirectories(view.getParent());
  }

  @Test
  @DisplayName("Should take no lock for a step that adds no file")
  void shouldTakeNoLockWithoutFile() {
    Long id = journal.openStep().close();

    assertNull(id);
    assertFalse(Files.exists(getLockFile(store)));
  }

  @Test
  @DisplayName("Should use and keep a lock file an earlier run left behind")
  void shouldReuseLockFileLeftBehind() throws IOException {
    Files.createDirectories(getLockFile(store).getParent());
    Files.writeString(getLockFile(store), "content an earlier version wrote\n");
    Object key = Files.readAttributes(getLockFile(store), BasicFileAttributes.class).fileKey();

    assertNotNull(new HistoryJournal(store, 100, 50).getInfo());
    assertTrue(Files.exists(getLockFile(store)));
    assertEquals(key,
        Files.readAttributes(getLockFile(store), BasicFileAttributes.class).fileKey());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should let exactly one of two journals hold the journal at a time")
  void shouldLetOneJournalHoldAtOnce() throws Exception {
    AtomicInteger holders = new AtomicInteger();
    AtomicInteger most = new AtomicInteger();
    write(view, toBytes("one\n"));
    for (int round = 0; round < 50; round++) {
      CountDownLatch start = new CountDownLatch(1);
      List<Thread> waiters = new ArrayList<>();
      for (int waiter = 0; waiter < 2; waiter++) {
        HistoryJournal target = new HistoryJournal(store);
        waiters.add(new Thread(() -> {
          try {
            start.await();
          } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
          }
          addStep(target, List.of(view), () -> {
            most.accumulateAndGet(holders.incrementAndGet(), Math::max);
            Thread.yield();
            holders.decrementAndGet();
            return null;
          });
        }));
      }
      waiters.forEach(Thread::start);
      start.countDown();
      for (Thread waiter : waiters) {
        waiter.join();
      }
    }

    assertEquals(1, most.get());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should refuse as busy a call the write of a record makes on the same thread")
  void shouldRefuseNestedCall() throws IOException {
    write(view, toBytes("one\n"));
    List<HistoryJournalInfo> nested = new ArrayList<>();

    addStep(journal, List.of(view), () -> {
      write(view, toBytes("two\n"));
      nested.add(journal.getInfo());
      return null;
    });

    assertEquals(Collections.singletonList(null), nested);
    assertEquals(1, journal.getInfo().getEntries().size());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should let a step that meets a busy journal record nothing and let the write run")
  void shouldRecordNothingWhenStepMeetsBusyJournal() throws Exception {
    Path other = project.resolve("Other.java");
    write(view, toBytes("one\n"));
    write(other, toBytes("other one\n"));
    CountDownLatch holding = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    Thread holder = new Thread(() -> addWrite(journal, view, toBytes("two\n"), holding, release));
    holder.start();
    holding.await();

    Long id;
    try {
      HistoryStep step = new HistoryJournal(store, 100, 50).openStep();
      step.addFile(other);
      write(other, toBytes("other two\n"));
      id = step.close();
    } finally {
      release.countDown();
      holder.join();
    }

    assertNull(id);
    assertArrayEquals(toBytes("other two\n"), Files.readAllBytes(other));
    assertEquals(1, journal.getInfo().getEntries().size());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should answer the id once and ignore a capture after the step closed")
  void shouldCloseStepOnce() {
    write(view, toBytes("one\n"));
    HistoryStep step = journal.openStep();
    step.addFile(view);
    write(view, toBytes("two\n"));

    Long id = step.close();
    step.addFile(view);

    assertNotNull(id);
    assertNull(step.close());
    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should refuse as busy while another process holds the lock file")
  void shouldRefuseWhileOtherProcessHolds() throws Exception {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Process holder = startLockProgram("hold");
    try {
      assertEquals("held", readLine(holder));

      HistoryRestoreResult busy = new HistoryJournal(store, 100, 50).undo();

      assertEquals(HistoryRestoreResult.Code.BUSY, busy.getCode());
      assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    } finally {
      stopLockProgram(holder);
    }

    assertTrue(journal.undo().isDone());
    assertTrue(Files.exists(getLockFile(store)));
  }

  @Test
  @DisplayName("Should wait while another process holds the lock file and go on once it lets go")
  void shouldWaitForOtherProcess() throws Exception {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    Process holder = startLockProgram("hold");
    CompletableFuture<HistoryRestoreResult> undo;
    try {
      assertEquals("held", readLine(holder));

      undo = CompletableFuture.supplyAsync(() -> journal.undo());

      assertThrows(TimeoutException.class, () -> undo.get(300, TimeUnit.MILLISECONDS));
      assertArrayEquals(toBytes("two\n"), Files.readAllBytes(view));
    } finally {
      stopLockProgram(holder);
    }

    assertTrue(undo.get().isDone());
    assertArrayEquals(toBytes("one\n"), Files.readAllBytes(view));
    assertTrue(Files.exists(getLockFile(store)));
  }

  @Test
  @DisplayName("Should leave the lock file free for another process once a call returns")
  void shouldLeaveLockFreeForOtherProcess() throws Exception {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    assertTrue(journal.undo().isDone());

    Process probe = startLockProgram("probe");

    assertEquals("free", readLine(probe));
    assertEquals(0, probe.waitFor());
  }

  @Test
  @DisplayName("Should release the lock after a refusal")
  void shouldReleaseLockAfterRefusal() throws IOException {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    write(view, toBytes("outside\n"));

    assertEquals(HistoryRestoreResult.Code.FILE_CHANGED, journal.undo().getCode());
    assertTrue(isLockFree(store));
  }

  @Test
  @DisplayName("Should release the lock when the write throws an error")
  void shouldReleaseLockOnError() throws IOException {
    write(view, toBytes("one\n"));

    List<Path> files = List.of(view);
    Supplier<Object> failing = () -> {
      write(view, toBytes("two\n"));
      throw new WriteError();
    };

    assertThrows(WriteError.class, () -> addStep(journal, files, failing));

    assertTrue(isLockFree(store));
    assertEquals(1, journal.getInfo().getEntries().size());
  }

  @Test
  @DisplayName("Should answer NOTHING_TO_REDO to a redo that waited while a step replaced it")
  void shouldRefuseRedoThatWaitedForStep() throws Exception {
    write(view, toBytes("one\n"));
    addWrite(journal, view, toBytes("two\n"));
    final long undone = journal.getInfo().getEntries().get(0).getId();
    assertTrue(journal.undo().isDone());
    HistoryJournal recording = new HistoryJournal(store);
    HistoryJournal redoing = new HistoryJournal(store);
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

    CompletableFuture<HistoryRestoreResult> redo =
        CompletableFuture.supplyAsync(() -> redoing.redo(undone));

    assertThrows(TimeoutException.class, () -> redo.get(200, TimeUnit.MILLISECONDS));

    release.countDown();
    holder.join();

    assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, redo.get().getCode());
    assertArrayEquals(toBytes("three\n"), Files.readAllBytes(view));
  }

  private Process startLockProgram(String mode) throws IOException {
    Path program = programs.resolve("LockProgram.java");
    Files.writeString(program, """
        import java.nio.channels.FileChannel;
        import java.nio.channels.FileLock;
        import java.nio.file.Path;
        import java.nio.file.StandardOpenOption;

        public class LockProgram {
          public static void main(String[] args) throws Exception {
            try (FileChannel channel = FileChannel.open(Path.of(args[0]),
                StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
              if (args[1].equals("hold")) {
                channel.lock();
                System.out.println("held");
                System.in.read();
              } else {
                FileLock lock = channel.tryLock();
                System.out.println(lock == null ? "taken" : "free");
              }
            }
          }
        }
        """);
    Files.createDirectories(getLockFile(store).getParent());
    String java = Path.of(System.getProperty("java.home"), "bin", "java").toString();

    return new ProcessBuilder(java, program.toString(), getLockFile(store).toString(), mode)
        .redirectError(Redirect.INHERIT).start();
  }

  private String readLine(Process process) throws IOException {
    try (BufferedReader reader = new BufferedReader(
        new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
      return reader.readLine();
    }
  }

  private void stopLockProgram(Process process) throws Exception {
    try (OutputStream in = process.getOutputStream()) {
      in.write('\n');
    }

    assertTrue(process.waitFor(30, TimeUnit.SECONDS));
  }

  private static final class WriteError extends Error {

    private static final long serialVersionUID = 1L;
  }
}
