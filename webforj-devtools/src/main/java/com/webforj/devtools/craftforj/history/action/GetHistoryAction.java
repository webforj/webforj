package com.webforj.devtools.craftforj.history.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;

/**
 * Gets the undo and redo journal of the running application.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class GetHistoryAction implements CraftforjActionHandler<HistoryJournalInfo> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "history.get";

  private final HistoryJournal journal;

  /**
   * Creates the action.
   *
   * @param journal the journal of the application
   */
  public GetHistoryAction(HistoryJournal journal) {
    this.journal = journal;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getAction() {
    return ACTION;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public HistoryJournalInfo handle(JsonObject params) {
    HistoryJournalInfo info = journal.getInfo();
    if (info == null) {
      throw new CraftforjActionException("The history is busy or could not be read");
    }

    return info;
  }
}
