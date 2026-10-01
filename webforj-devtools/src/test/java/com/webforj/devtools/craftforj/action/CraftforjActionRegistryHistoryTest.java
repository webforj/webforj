package com.webforj.devtools.craftforj.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.Page;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.ProjectFileWriter;
import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import com.webforj.devtools.craftforj.inspector.action.ApplyStagedSourceAction;
import com.webforj.devtools.craftforj.security.ChannelCredentials;
import com.webforj.devtools.craftforj.source.staging.SourceHasher;
import com.webforj.devtools.craftforj.source.staging.SourceStagingArea;
import com.webforj.devtools.craftforj.source.staging.model.StagedFile;
import com.webforj.devtools.craftforj.styles.StylesheetModifier;
import com.webforj.devtools.craftforj.styles.StylesheetResolver;
import com.webforj.devtools.craftforj.styles.action.WriteStylesheetAction;
import com.webforj.event.page.PageEvent;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.ArgumentCaptor;

@DisplayName("CraftforjActionRegistry history step")
class CraftforjActionRegistryHistoryTest {

  private static final ChannelCredentials CREDENTIALS =
      ChannelCredentials.of("test-nonce", "sink1");

  @TempDir
  Path project;

  @TempDir
  Path home;

  private HistoryJournal journal;
  private CraftforjActionRegistry registry;

  @BeforeEach
  void setUp() {
    journal = HistoryJournal.create(home, project, CraftforjActionRegistryHistoryTest.class);
    registry = new CraftforjActionRegistry(CREDENTIALS);
    registry.setHistory(journal);
  }

  @Test
  @DisplayName("Should record one request writing two files as one entry one undo takes back")
  void shouldRecordTwoFilesAsOneEntry() throws IOException {
    Path first = write(project.resolve("First.java"), "class First {}\n");
    Path second = write(project.resolve("Second.java"), "class Second {}\n");
    Map<String, StagedFile> staged = new LinkedHashMap<>();
    SourceStagingArea staging = new SourceStagingArea(() -> staged);
    staging.stage(new StagedFile(first.toString(), SourceHasher.hash("class First {}\n"),
        "class First { int a; }\n", false, true));
    staging.stage(new StagedFile(second.toString(), SourceHasher.hash("class Second {}\n"),
        "class Second { int b; }\n", false, true));
    registry.register(new ApplyStagedSourceAction(staging));

    JsonObject response = dispatch("inspector.applyStagedSource", new JsonObject());

    List<HistoryEntryInfo> entries = journal.getInfo().getEntries();
    assertEquals(1, entries.size());
    assertEquals(entries.get(0).getId(), response.get("historyId").getAsLong());
    assertEquals(List.of(first.toString(), second.toString()),
        entries.get(0).getFiles().stream().map(HistoryFileSnapshot::getPath).toList());

    assertTrue(journal.undo(null).isDone());
    assertEquals("class First {}\n", Files.readString(first));
    assertEquals("class Second {}\n", Files.readString(second));
  }

  @Test
  @DisplayName("Should record a write action that knows nothing of the history")
  void shouldRecordFakeWriteAction() throws IOException {
    Path file = write(project.resolve("Fake.java"), "one\n");
    registry.register(new FakeWriteAction(file, "two\n"));

    JsonObject response = dispatch("fake.write", new JsonObject());

    HistoryJournalInfo info = journal.getInfo();
    assertEquals(1, info.getEntries().size());
    assertEquals(info.getEntries().get(0).getId(), response.get("historyId").getAsLong());
    assertTrue(response.get("success").getAsBoolean());
    assertTrue(journal.undo(null).isDone());
    assertEquals("one\n", Files.readString(file));
    assertTrue(journal.redo(null).isDone());
    assertEquals("two\n", Files.readString(file));
  }

  @Test
  @DisplayName("Should leave no entry for a dry run")
  void shouldLeaveNoEntryForDryRun() throws IOException {
    Path sheet = write(project.resolve("src/main/frontend/app.css"), "a {}\n");
    registry.register(
        new WriteStylesheetAction(new StylesheetResolver(project), new StylesheetModifier()));
    JsonObject params = createAppendParams(sheet);
    params.addProperty("dryRun", true);

    JsonObject response = dispatch(WriteStylesheetAction.ACTION, params);

    assertTrue(response.get("success").getAsBoolean());
    assertTrue(response.get("historyId").isJsonNull());
    assertEquals("a {}\n", Files.readString(sheet));
    assertNull(journal.getInfo().getJournalId());
  }

  @Test
  @DisplayName("Should record a stylesheet write through its atomic replace")
  void shouldRecordStylesheetWrite() throws IOException {
    Path sheet = write(project.resolve("src/main/frontend/app.css"), "a {}\n");
    registry.register(
        new WriteStylesheetAction(new StylesheetResolver(project), new StylesheetModifier()));

    JsonObject response = dispatch(WriteStylesheetAction.ACTION, createAppendParams(sheet));

    assertEquals(journal.getInfo().getEntries().get(0).getId(),
        response.get("historyId").getAsLong());
    assertTrue(journal.undo(null).isDone());
    assertEquals("a {}\n", Files.readString(sheet));
  }

  @Test
  @DisplayName("Should keep the mode of a stylesheet through a write, an undo and a redo")
  void shouldKeepStylesheetModeThroughUndoAndRedo() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));
    Path sheet = write(project.resolve("src/main/frontend/app.css"), "a {}\n");
    Files.setPosixFilePermissions(sheet, PosixFilePermissions.fromString("rw-rw-r--"));
    registry.register(
        new WriteStylesheetAction(new StylesheetResolver(project), new StylesheetModifier()));

    dispatch(WriteStylesheetAction.ACTION, createAppendParams(sheet));

    assertEquals("rw-rw-r--", getMode(sheet));
    assertTrue(journal.undo(null).isDone());
    assertEquals("a {}\n", Files.readString(sheet));
    assertEquals("rw-rw-r--", getMode(sheet));
    assertTrue(journal.redo(null).isDone());
    assertEquals("rw-rw-r--", getMode(sheet));
  }

  @Test
  @DisplayName("Should leave no entry for a write that failed and rolled back")
  void shouldLeaveNoEntryForRolledBackWrite() throws IOException {
    Path first = write(project.resolve("First.java"), "class First {}\n");
    Path blocker = write(project.resolve("blocker"), "a file where a folder is needed\n");
    Map<String, StagedFile> staged = new LinkedHashMap<>();
    SourceStagingArea staging = new SourceStagingArea(() -> staged);
    staging.stage(new StagedFile(first.toString(), SourceHasher.hash("class First {}\n"),
        "class First { int a; }\n", false, true));
    staging.stage(new StagedFile(blocker.resolve("Second.java").toString(), null,
        "class Second {}\n", true, true));
    registry.register(new ApplyStagedSourceAction(staging));

    JsonObject response = dispatch("inspector.applyStagedSource", new JsonObject());

    assertEquals("APPLY_FAILED", response.getAsJsonObject("data").get("code").getAsString());
    assertTrue(response.get("historyId").isJsonNull());
    assertEquals("class First {}\n", Files.readString(first));
    assertTrue(journal.getInfo().getEntries().isEmpty());
  }

  @Test
  @DisplayName("Should leave no entry for a handler that writes nothing")
  void shouldLeaveNoEntryWithoutWrite() {
    registry.register(new ReadAction());

    JsonObject response = dispatch("fake.read", new JsonObject());

    assertTrue(response.get("success").getAsBoolean());
    assertTrue(response.get("historyId").isJsonNull());
    assertNull(journal.getInfo().getJournalId());
  }

  @Test
  @DisplayName("Should answer the entry of a request that failed after it wrote")
  void shouldAnswerEntryOfFailedRequest() throws IOException {
    Path file = write(project.resolve("Fake.java"), "one\n");
    registry.register(new FakeWriteAction(file, "two\n") {
      @Override
      public String handle(JsonObject params) {
        super.handle(params);
        throw new CraftforjActionException("failed after the write");
      }
    });

    JsonObject response = dispatch("fake.write", new JsonObject());

    assertFalse(response.get("success").getAsBoolean());
    assertEquals(journal.getInfo().getEntries().get(0).getId(),
        response.get("historyId").getAsLong());
  }

  @Test
  @DisplayName("Should record nothing and answer no id without a history")
  void shouldRecordNothingWithoutHistory() throws IOException {
    Path file = write(project.resolve("Fake.java"), "one\n");
    registry.setHistory(null);
    registry.register(new FakeWriteAction(file, "two\n"));

    JsonObject response = dispatch("fake.write", new JsonObject());

    assertEquals("two\n", Files.readString(file));
    assertTrue(response.get("historyId").isJsonNull());
    assertNull(journal.getInfo().getJournalId());
  }

  @Test
  @DisplayName("Should record nothing for a write made after the request ended")
  void shouldClearObserverAfterRequest() throws IOException {
    Path file = write(project.resolve("Fake.java"), "one\n");
    registry.register(new FakeWriteAction(file, "two\n"));
    dispatch("fake.write", new JsonObject());

    ProjectFileWriter.write(file, "three\n");

    assertEquals(1, journal.getInfo().getEntries().size());
    assertEquals(journal.getInfo().getEntries().get(0).getFiles().get(0).getAfter(),
        SourceHasher.hash("two\n"));
  }

  @Test
  @DisplayName("Should record two requests on two threads as two entries in order")
  void shouldRecordRequestsOfTwoThreads() throws Exception {
    Path first = write(project.resolve("First.java"), "first one\n");
    Path second = write(project.resolve("Second.java"), "second one\n");
    CountDownLatch written = new CountDownLatch(1);
    CountDownLatch release = new CountDownLatch(1);
    registry.register(new FakeWriteAction(first, "first two\n") {
      @Override
      public String handle(JsonObject params) {
        super.handle(params);
        written.countDown();
        try {
          release.await();
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }

        return "written";
      }
    });
    registry.register(new FakeWriteAction(second, "second two\n") {
      @Override
      public String getAction() {
        return "fake.writeSecond";
      }
    });

    final CompletableFuture<JsonObject> held =
        CompletableFuture.supplyAsync(() -> dispatch("fake.write", new JsonObject()));
    written.await();
    CompletableFuture<JsonObject> waiting =
        CompletableFuture.supplyAsync(() -> dispatch("fake.writeSecond", new JsonObject()));
    assertThrows(TimeoutException.class, () -> waiting.get(200, TimeUnit.MILLISECONDS));

    release.countDown();
    long firstId = held.get().get("historyId").getAsLong();
    long secondId = waiting.get().get("historyId").getAsLong();

    assertNotEquals(firstId, secondId);
    assertTrue(firstId < secondId);
    assertEquals(List.of(secondId, firstId),
        journal.getInfo().getEntries().stream().map(HistoryEntryInfo::getId).toList());
    assertTrue(journal.undo(null).isDone());
    assertTrue(journal.undo(null).isDone());
    assertEquals("first one\n", Files.readString(first));
    assertEquals("second one\n", Files.readString(second));
  }

  private JsonObject dispatch(String action, JsonObject params) {
    JsonObject request = new JsonObject();
    request.addProperty("requestId", "r1");
    request.addProperty("action", action);
    request.addProperty("nonce", CREDENTIALS.getNonce());
    request.add("params", params);
    Page page = mock(Page.class);
    PageEvent event = mock(PageEvent.class);
    when(event.getData()).thenReturn(Map.of("request", request.toString()));

    registry.dispatch(page, event);

    ArgumentCaptor<String> script = ArgumentCaptor.forClass(String.class);
    verify(page).executeJsVoidAsync(script.capture());
    String sent = script.getValue();

    return JsonParser.parseString(sent.substring(sent.indexOf('(') + 1, sent.lastIndexOf(')')))
        .getAsJsonObject();
  }

  private static JsonObject createAppendParams(Path sheet) {
    JsonObject change = new JsonObject();
    change.addProperty("type", "APPEND");
    change.addProperty("text", ".card { padding: 8px; }");
    JsonArray changes = new JsonArray();
    changes.add(change);
    JsonObject params = new JsonObject();
    params.addProperty("file", "src/main/frontend/" + sheet.getFileName());
    params.add("changes", changes);

    return params;
  }

  private static String getMode(Path file) throws IOException {
    return PosixFilePermissions.toString(Files.getPosixFilePermissions(file));
  }

  private static Path write(Path file, String content) throws IOException {
    Files.createDirectories(file.getParent());
    Files.writeString(file, content);

    return file;
  }

  private static class FakeWriteAction implements CraftforjActionHandler<String> {

    private final Path file;
    private final String content;

    FakeWriteAction(Path file, String content) {
      this.file = file;
      this.content = content;
    }

    @Override
    public String getAction() {
      return "fake.write";
    }

    @Override
    public String handle(JsonObject params) {
      try {
        ProjectFileWriter.write(file, content);
      } catch (IOException e) {
        throw new UncheckedIOException(e);
      }

      return "written";
    }
  }

  private static class ReadAction implements CraftforjActionHandler<String> {

    @Override
    public String getAction() {
      return "fake.read";
    }

    @Override
    public String handle(JsonObject params) {
      return "read";
    }
  }
}
