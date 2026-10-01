package com.webforj.devtools.craftforj.history.action;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.history.HistoryJournal;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult;
import com.webforj.devtools.craftforj.history.model.HistoryRestoreResult.Code;
import java.util.OptionalLong;
import java.util.function.Function;

/**
 * Handles the requests of the undo and the redo action.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class HistoryRestoreRequests {

  private static final String PARAM_ID = "id";

  private HistoryRestoreRequests() {}

  /**
   * Handles an undo or a redo on the step a request names.
   *
   * @param params the request parameters
   * @param journal the journal of the application
   * @param step the undo or the redo, given the id of the step or {@code null} for the next one
   * @param refusal the code a request with an id that is not a whole number is refused with
   * @return the outcome
   */
  static HistoryRestoreResult handle(JsonObject params, HistoryJournal journal,
      Function<Long, HistoryRestoreResult> step, Code refusal) {
    JsonElement named = params == null ? null : params.get(PARAM_ID);
    if (named == null || named.isJsonNull()) {
      return step.apply(null);
    }

    OptionalLong entryId = readEntryId(named);
    if (entryId.isEmpty()) {
      return HistoryRestoreResult.Builder.create().setCode(refusal)
          .setMessage("The entry id is not a whole number, nothing was written")
          .setJournal(journal.getInfo()).build();
    }

    return step.apply(entryId.getAsLong());
  }

  static OptionalLong readEntryId(JsonElement element) {
    if (element == null || !element.isJsonPrimitive() || !element.getAsJsonPrimitive().isNumber()) {
      return OptionalLong.empty();
    }

    try {
      return OptionalLong.of(element.getAsBigDecimal().longValueExact());
    } catch (ArithmeticException e) {
      return OptionalLong.empty();
    }
  }
}
