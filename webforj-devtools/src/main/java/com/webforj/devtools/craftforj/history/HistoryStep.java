package com.webforj.devtools.craftforj.history;

import com.webforj.devtools.craftforj.history.HistoryLock.Hold;
import java.io.IOException;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * One request as the history records it. The project files it changed become one journal entry.
 *
 * <p>
 * Recording never fails the request. When the journal is busy or a file cannot be read, the step
 * records nothing.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class HistoryStep {

  private static final Logger LOGGER = System.getLogger(HistoryStep.class.getName());

  private final HistoryJournal journal;
  private final Map<Path, byte[]> before = new LinkedHashMap<>();
  private Hold hold;
  private boolean failed;
  private boolean closed;

  HistoryStep(HistoryJournal journal) {
    this.journal = journal;
  }

  /**
   * Closes the step and adds the files that changed to the journal as one entry.
   *
   * @return the id of the entry, or {@code null} when nothing was recorded
   */
  public Long close() {
    if (closed) {
      return null;
    }

    closed = true;
    ProjectFileWriter.clearStep();
    if (hold == null) {
      return null;
    }

    try (Hold held = hold) {
      return failed ? null : journal.addEntry(before);
    }
  }

  void addFile(Path file) {
    Path path = file.toAbsolutePath().normalize();
    if (closed || failed || before.containsKey(path)) {
      return;
    }

    try {
      if (hold == null) {
        hold = journal.createHold();
      }

      before.put(path, HistoryJournal.readFile(path));
    } catch (IOException e) {
      LOGGER.log(Level.WARNING, "The history is busy or a file could not be read, the change of "
          + path + " is not recorded", e);
      failed = true;
    }
  }
}
