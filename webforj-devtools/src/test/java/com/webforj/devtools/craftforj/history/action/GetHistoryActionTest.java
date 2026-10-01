package com.webforj.devtools.craftforj.history.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

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

@DisplayName("GetHistoryAction")
class GetHistoryActionTest {

  @TempDir
  Path dir;

  private HistoryJournal journal;
  private Path file;

  @BeforeEach
  void setUp() throws IOException {
    journal = HistoryJournal.create(dir.resolve("home"), dir, GetHistoryActionTest.class);
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
      assertEquals("history.get", new GetHistoryAction(journal).getAction());
    }
  }

  @Nested
  @DisplayName("handle")
  class Handle {

    @Test
    @DisplayName("Should read the journal")
    void shouldReadJournal() {
      assertEquals(1, new GetHistoryAction(journal).handle(new JsonObject()).getEntries().size());
    }

    @Test
    @DisplayName("Should fail rather than answer an empty journal when the history cannot be read")
    void shouldFailWhenHistoryCannotBeRead() throws IOException {
      Path blocked = dir.resolve("blocked");
      Files.writeString(blocked, "not a directory");
      HistoryJournal unusable = HistoryJournal.create(blocked, dir, GetHistoryActionTest.class);

      assertEquals("The history is busy or could not be read",
          assertThrows(CraftforjActionException.class,
              () -> new GetHistoryAction(unusable).handle(new JsonObject())).getMessage());
    }
  }
}
