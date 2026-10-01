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

@DisplayName("RedoHistoryAction")
class RedoHistoryActionTest {

  @TempDir
  Path dir;

  private HistoryJournal journal;
  private Path file;

  @BeforeEach
  void setUp() throws IOException {
    journal = HistoryJournal.create(dir.resolve("home"), dir, RedoHistoryActionTest.class);
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
      assertEquals("history.redo", new RedoHistoryAction(journal).getAction());
    }
  }

  @Nested
  @DisplayName("handle")
  class Handle {

    @Test
    @DisplayName("Should redo the most recently undone step")
    void shouldRedoUndoneStep() throws IOException {
      assertTrue(journal.undo(null).isDone());

      assertTrue(new RedoHistoryAction(journal).handle(new JsonObject()).isDone());
      assertArrayEquals("two\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should redo the entry a request names")
    void shouldRedoNamedEntry() throws IOException {
      long id = journal.getInfo().getEntries().get(0).getId();
      JsonObject named = new JsonObject();
      named.addProperty("id", id);
      assertTrue(journal.undo(id).isDone());

      assertTrue(new RedoHistoryAction(journal).handle(named).isDone());
      assertArrayEquals("two\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should take the most recently undone step when a request names no entry or a null "
        + "one")
    void shouldTakeNewestWithoutEntryId() throws IOException {
      JsonObject none = new JsonObject();
      none.add("id", JsonNull.INSTANCE);

      assertTrue(journal.undo(null).isDone());
      assertTrue(new RedoHistoryAction(journal).handle(none).isDone());
      assertTrue(journal.undo(null).isDone());
      assertTrue(new RedoHistoryAction(journal).handle(null).isDone());
      assertArrayEquals("two\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should refuse an entry id that is not a number and write nothing")
    void shouldRefuseEntryIdThatIsNoNumber() throws IOException {
      JsonObject odd = new JsonObject();
      odd.add("id", new JsonObject());
      assertTrue(journal.undo(null).isDone());

      HistoryRestoreResult result = new RedoHistoryAction(journal).handle(odd);

      assertFalse(result.isDone());
      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, result.getCode());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
      assertFalse(journal.getInfo().getEntries().get(0).isApplied());
    }

    @Test
    @DisplayName("Should refuse an entry id that is text and write nothing")
    void shouldRefuseEntryIdThatIsText() throws IOException {
      JsonObject text = new JsonObject();
      text.addProperty("id", "abc");
      assertTrue(journal.undo(null).isDone());

      HistoryRestoreResult result = new RedoHistoryAction(journal).handle(text);

      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, result.getCode());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
    }

    @Test
    @DisplayName("Should refuse an entry id that is not a whole number and write nothing")
    void shouldRefuseEntryIdThatIsFraction() throws IOException {
      Path other = dir.resolve("Other.java");
      Files.writeString(other, "one\n");
      HistoryStep step = journal.openStep();
      ProjectFileWriter.write(other, "two\n");
      step.close();
      long second = journal.getInfo().getEntries().get(0).getId();
      long first = journal.getInfo().getEntries().get(1).getId();
      assertTrue(journal.undo(second).isDone());
      assertTrue(journal.undo(first).isDone());
      JsonObject fraction = new JsonObject();
      fraction.addProperty("id", first + 0.5);

      HistoryRestoreResult result = new RedoHistoryAction(journal).handle(fraction);

      assertEquals(HistoryRestoreResult.Code.NOTHING_TO_REDO, result.getCode());
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(file));
      assertArrayEquals("one\n".getBytes(StandardCharsets.UTF_8), Files.readAllBytes(other));
    }
  }
}
