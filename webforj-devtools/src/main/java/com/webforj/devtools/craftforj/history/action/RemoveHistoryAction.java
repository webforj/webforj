package com.webforj.devtools.craftforj.history.action;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.action.CraftforjActionHandler;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.model.HistoryJournalInfo;
import java.util.ArrayList;
import java.util.List;

/**
 * Removes history steps, so they are no longer offered.
 *
 * <p>
 * The request names the steps by {@code ids}. An id that is not a whole number is left out.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class RemoveHistoryAction implements CraftforjActionHandler<HistoryJournalInfo> {

  /**
   * The action name for this handler.
   */
  public static final String ACTION = "history.remove";

  private static final String PARAM_IDS = "ids";

  private final HistoryJournal journal;

  /**
   * Creates the action.
   *
   * @param journal the journal of the application
   */
  public RemoveHistoryAction(HistoryJournal journal) {
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
    List<Long> ids = new ArrayList<>();
    if (params != null && params.has(PARAM_IDS) && params.get(PARAM_IDS).isJsonArray()) {
      for (JsonElement id : params.getAsJsonArray(PARAM_IDS)) {
        HistoryRestoreRequests.readEntryId(id).ifPresent(ids::add);
      }
    }

    HistoryJournalInfo info = journal.remove(ids);
    if (info == null) {
      throw new CraftforjActionException("The history is busy or could not be read");
    }

    return info;
  }
}
