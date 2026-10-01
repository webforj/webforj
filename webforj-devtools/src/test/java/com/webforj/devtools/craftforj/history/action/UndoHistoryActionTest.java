package com.webforj.devtools.craftforj.history.action;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.HistoryStep;
import com.webforj.devtools.craftforj.history.ProjectFileWriter;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("UndoHistoryAction")
class UndoHistoryActionTest {

  @TempDir
  Path dir;

  private HistoryJournal journal;
  private Path file;

  @BeforeEach
  void setUp() throws IOException {
    journal = HistoryJournal.create(dir.resolve("home"), dir, UndoHistoryActionTest.class);
    file = dir.resolve("View.java");
    Files.writeString(file, "one\n");
    HistoryStep step = journal.openStep();
    ProjectFileWriter.write(file, "two\n");
    step.close();
  }

  @Nested
  @DisplayName("getAction")
  class GetAction {

    @Test
    @DisplayName("Should name the action")
    void shouldNameAction() {
      assertEquals("history.undo", new UndoHistoryAction(journal).getAction());
    }
  }

  @Nested
  @DisplayName("handle")
  class Handle {

    @Test
    @DisplayName("Should undo the newest step")
    void shouldUndoNewestStep() throws IOException {
      assertTrue(new UndoHistoryAction(journal).handle(new JsonObject()).isDone());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should undo the entry a request names")
    void shouldUndoNamedEntry() throws IOException {
      long id = journal.getInfo().getEntries().get(0).getId();
      JsonObject named = new JsonObject();
      named.addProperty("id", id);

      assertTrue(new UndoHistoryAction(journal).handle(named).isDone());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should take the newest step when a request names no entry or a null one")
    void shouldTakeNewestWithoutEntryId() throws IOException {
      JsonObject none = new JsonObject();
      none.add("id", JsonNull.INSTANCE);

      assertTrue(new UndoHistoryAction(journal).handle(null).isDone());
      assertTrue(journal.redo().isDone());
      assertTrue(new UndoHistoryAction(journal).handle(none).isDone());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should refuse an entry id that is not a number and write nothing")
    void shouldRefuseEntryIdThatIsNoNumber() throws IOException {
      JsonObject odd = new JsonObject();
      odd.add("id", new JsonObject());

      HistoryRestoreResult result = new UndoHistoryAction(journal).handle(odd);

      assertFalse(result.isDone());
      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, result.getCode());
      assertArrayEquals("two\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
      assertTrue(journal.getInfo().getEntries().get(0).isApplied());
    }

    @Test
    @DisplayName("Should refuse an entry id that is text and write nothing")
    void shouldRefuseEntryIdThatIsText() throws IOException {
      JsonObject text = new JsonObject();
      text.addProperty("id", "abc");

      HistoryRestoreResult result = new UndoHistoryAction(journal).handle(text);

      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, result.getCode());
      assertArrayEquals("two\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should refuse an entry id that is not a whole number and write nothing")
    void shouldRefuseEntryIdThatIsFraction() throws IOException {
      HistoryStep step = journal.openStep();
      ProjectFileWriter.write(file, "three\n");
      step.close();
      long first = journal.getInfo().getEntries().get(1).getId();
      JsonObject fraction = new JsonObject();
      fraction.addProperty("id", first + 0.5);

      HistoryRestoreResult result = new UndoHistoryAction(journal).handle(fraction);

      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_UNDO, result.getCode());
      assertArrayEquals("three\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }
  }
}
