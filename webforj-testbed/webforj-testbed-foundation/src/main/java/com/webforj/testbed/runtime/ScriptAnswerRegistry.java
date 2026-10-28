package com.webforj.testbed.runtime;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.function.Consumer;

/**
 * The answers the client gives to scripts: the scripted results of {@code executeJs} and the
 * scripts started with {@code executeJsAsync} that wait for their result.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ScriptAnswerRegistry {
  private final Deque<Consumer<Object>> waiting = new ArrayDeque<>();
  private final Deque<Object> scripted = new ArrayDeque<>();

  /**
   * Creates a registry with no scripted result and no script waiting.
   */
  public ScriptAnswerRegistry() {
    // both queues are empty from their field initializers
  }

  /**
   * Scripts the result the next call of {@code executeJs} returns.
   *
   * @param result the result the client returns
   */
  public void answerExecuteJs(Object result) {
    scripted.add(new Answer(result));
  }

  /**
   * Completes the oldest script started with {@code executeJsAsync} that still waits for its
   * result.
   *
   * @param result the result the client returns
   *
   * @throws IllegalStateException if no script waits for a result
   */
  public void answerExecuteJsAsync(Object result) {
    Consumer<Object> completion = waiting.poll();
    if (completion == null) {
      throw new IllegalStateException("No script started with executeJsAsync waits for a result. "
          + "Start the script before answering it.");
    }

    completion.accept(result);
  }

  /**
   * Returns the scripted result of {@code executeJs}, null when none was scripted.
   *
   * @return the result the client returns
   */
  public Object nextExecuteJsResult() {
    Answer answer = (Answer) scripted.poll();

    return answer == null ? null : answer.result;
  }

  /**
   * Registers a script started with {@code executeJsAsync} until the test answers it.
   *
   * @param completion what completes the script with the result
   */
  public void await(Consumer<Object> completion) {
    waiting.add(completion);
  }

  private static final class Answer {
    private final Object result;

    private Answer(Object result) {
      this.result = result;
    }
  }
}
