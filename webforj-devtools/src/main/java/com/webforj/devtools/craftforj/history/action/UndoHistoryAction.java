package com.webforj.devtools.craftforj.history.action;

import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult.Code;

/**
 * Takes an applied history step back.
 *
 * <p>
 * The request names the step by {@code id}, or nothing for the next step. An id that is not a whole
 * number is refused.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class UndoHistoryAction implements CraftforjActionHandler<HistoryRestoreResult> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "history.undo";

  private final HistoryJournal journal;

  /**
   * Creates the action.
   *
   * @param journal the journal of the application
   */
  public UndoHistoryAction(HistoryJournal journal) {
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
  public HistoryRestoreResult handle(JsonObject params) {
    return HistoryRestoreRequests.handle(params, journal, journal::undo, Code.NOTHING_TO_UNDO);
  }
}
