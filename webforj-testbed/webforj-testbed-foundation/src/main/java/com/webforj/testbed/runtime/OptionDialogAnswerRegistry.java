package com.webforj.testbed.runtime;

import com.webforj.component.optiondialog.ConfirmDialog;
import com.webforj.testbed.ShownOptionDialog;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

/**
 * The scripted answers to option dialogs and the record of the dialogs shown.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class OptionDialogAnswerRegistry {
  private static final String CANCEL = "::CANCEL::";

  private final Map<ShownOptionDialog.Kind, Deque<Answer>> answers =
      new EnumMap<>(ShownOptionDialog.Kind.class);
  private final List<ShownOptionDialog> shown = new ArrayList<>();

  OptionDialogAnswerRegistry() {}

  /**
   * Scripts the answer to the next dialog of the given kind.
   *
   * @param kind the dialog kind
   * @param answer the answer, a {@link ConfirmDialog.Result} for confirm dialogs and a string or
   *        null for the others
   */
  public void answer(ShownOptionDialog.Kind kind, Object answer) {
    answers.computeIfAbsent(kind, key -> new ArrayDeque<>()).add(new Answer(answer));
  }

  /**
   * Returns the dialogs shown so far, in order.
   *
   * @return the shown dialogs
   */
  public List<ShownOptionDialog> getShown() {
    return List.copyOf(shown);
  }

  int confirm(ShownOptionDialog request, boolean singleButton) {
    Answer answer = next(request);
    if (answer == null) {
      if (singleButton) {
        return ConfirmDialog.Result.OK.getValue();
      }

      throw unanswered(request, "WebforjTestbed.getCurrent().answerConfirmDialog");
    }

    return ((ConfirmDialog.Result) answer.value).getValue();
  }

  String text(ShownOptionDialog request, String scriptedBy) {
    Answer answer = next(request);
    if (answer == null) {
      throw unanswered(request, scriptedBy);
    }

    return answer.value == null ? CANCEL : String.valueOf(answer.value);
  }

  private Answer next(ShownOptionDialog request) {
    shown.add(request);
    Deque<Answer> queue = answers.get(request.getKind());

    return queue == null ? null : queue.poll();
  }

  private static IllegalStateException unanswered(ShownOptionDialog request, String scriptedBy) {
    return new IllegalStateException(
        "The application showed a " + request + " and no answer was scripted. Call " + scriptedBy
            + " before the code under test shows the dialog.");
  }

  private static final class Answer {
    private final Object value;

    private Answer(Object value) {
      this.value = value;
    }
  }
}
