package com.webforj.devtools.craftforj.history.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.HistoryStep;
import com.webforj.devtools.craftforj.history.ProjectFileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

@DisplayName("RemoveHistoryAction")
class RemoveHistoryActionTest {

  @TempDir
  Path dir;

  private HistoryJournal journal;
  private Path file;

  @BeforeEach
  void setUp() throws IOException {
    journal = HistoryJournal.create(dir.resolve("home"), dir, RemoveHistoryActionTest.class);
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
      assertEquals("history.remove", new RemoveHistoryAction(journal).getAction());
    }
  }

  @Nested
  @DisplayName("handle")
  class Handle {

    @Test
    @DisplayName("Should remove the entries a request names")
    void shouldRemoveNamedEntries() {
      long id = journal.getInfo().getEntries().get(0).getId();
      JsonArray ids = new JsonArray();
      ids.add(id);
      ids.add("not a number");
      JsonObject remove = new JsonObject();
      remove.add("ids", ids);

      assertTrue(new RemoveHistoryAction(journal).handle(remove).getEntries().isEmpty());
      assertTrue(new RemoveHistoryAction(journal).handle(null).getEntries().isEmpty());
    }

    @Test
    @DisplayName("Should remove nothing for a request without a list of numbers")
    void shouldRemoveNothingWithoutIds() {
      JsonObject notList = new JsonObject();
      notList.addProperty("ids", 1);
      JsonArray odd = new JsonArray();
      odd.add(new JsonObject());
      JsonObject oddList = new JsonObject();
      oddList.add("ids", odd);

      assertEquals(1,
          new RemoveHistoryAction(journal).handle(new JsonObject()).getEntries().size());
      assertEquals(1, new RemoveHistoryAction(journal).handle(notList).getEntries().size());
      assertEquals(1, new RemoveHistoryAction(journal).handle(oddList).getEntries().size());
    }

    @Test
    @DisplayName("Should skip an id that is not a whole number instead of cutting it down")
    void shouldSkipIdThatIsFraction() {
      long id = journal.getInfo().getEntries().get(0).getId();
      JsonArray ids = new JsonArray();
      ids.add(id + 0.5);
      JsonObject remove = new JsonObject();
      remove.add("ids", ids);

      assertEquals(1, new RemoveHistoryAction(journal).handle(remove).getEntries().size());
    }

    @Test
    @DisplayName("Should fail rather than answer an empty journal when the history cannot be read")
    void shouldFailWhenHistoryCannotBeRead() throws IOException {
      Path blocked = dir.resolve("blocked");
      Files.writeString(blocked, "not a directory");
      HistoryJournal unusable = HistoryJournal.create(blocked, dir, RemoveHistoryActionTest.class);

      assertEquals("The history is busy or could not be read",
          assertThrows(CraftforjActionException.class,
              () -> new RemoveHistoryAction(unusable).handle(new JsonObject())).getMessage());
    }
  }
}
