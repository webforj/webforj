package com.webforj.data;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

/**
 * WorkflowExecutor orchestrates the execution of a series of asynchronous tasks. Tasks can include
 * conditional branches and sequential execution.
 *
 * @param <T> The type of context object passed between tasks, allowing them to share data.
 *
 * @see AsyncTask
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class WorkflowExecutor<T> {

  /**
   * The AsyncTask interface represents an individual task that can be executed asynchronously.
   * Implementations of this interface define specific actions to perform, possibly using and/or
   * modifying the provided context.
   *
   * @param <T> The type of context object shared among tasks.
   */
  public interface AsyncTask<T> {
    /**
     * Executes the task asynchronously.
     *
     * @param context The shared context object containing data for the task.
     * @param onComplete A callback to be invoked when the task completes. The argument indicates
     *        whether the workflow should continue with the next task.
     */
    void execute(T context, Consumer<Boolean> onComplete);
  }

  private final List<AsyncTask<T>> tasks;
  private final Consumer<T> completionCallback;
  private int currentIndex = 0;
  private boolean failed = false;

  /**
   * Constructs a new WorkflowExecutor with a list of tasks to execute.
   *
   * @param tasks The list of tasks to be executed in sequence.
   * @param completionCallback A callback to be invoked when all tasks have been completed.
   */
  public WorkflowExecutor(List<AsyncTask<T>> tasks, Consumer<T> completionCallback) {
    this.tasks = tasks != null ? tasks : new ArrayList<>();
    this.completionCallback = completionCallback;
  }

  /**
   * Constructs a new WorkflowExecutor with a list of tasks to execute.
   *
   * @param tasks The list of tasks to be executed in sequence.
   */
  public WorkflowExecutor(List<AsyncTask<T>> tasks) {
    this(tasks, null);
  }

  /**
   * Constructs a new WorkflowExecutor with an empty list of tasks.
   */
  public WorkflowExecutor() {
    this(new ArrayList<>(), null);
  }

  /**
   * Adds a new task to the workflow.
   *
   * @param task The task to be added.
   */
  public void addTask(AsyncTask<T> task) {
    tasks.add(task);
  }

  /**
   * Starts the execution of the workflow from the first task.
   *
   * @param context The shared context object to pass through the workflow.
   * @param onComplete The callback to be invoked when all tasks are completed.
   */
  public void run(T context, Consumer<T> onComplete) {
    if (tasks.isEmpty()) {
      if (completionCallback != null) {
        completionCallback.accept(context);
      }
      if (onComplete != null) {
        onComplete.accept(context);
      }
      return;
    }

    executeNext(context, onComplete);
  }

  /**
   * Starts the execution of the workflow from the first task.
   *
   * @param context The shared context object to pass through the workflow.
   */
  public void run(T context) {
    run(context, null);
  }

  /**
   * Conditionally branches the workflow based on a predicate.
   *
   * @param condition The condition to evaluate.
   * @param ifTrue The tasks to execute if the condition is true.
   * @param ifFalse The tasks to execute if the condition is false.
   *
   * @return A new WorkflowExecutor instance for the conditional branch.
   */
  public static <T> WorkflowExecutor<T> branch(Predicate<T> condition, List<AsyncTask<T>> ifTrue,
      List<AsyncTask<T>> ifFalse, Consumer<T> completionCallback) {
    return new WorkflowExecutor<>(List.of((context, onComplete) -> {
      if (condition.test(context)) {
        new WorkflowExecutor<>(ifTrue, completionCallback).run(context);
      } else {
        new WorkflowExecutor<>(ifFalse, completionCallback).run(context);
      }
      onComplete.accept(true);
    }), completionCallback);
  }

  /**
   * Conditionally branches the workflow based on a predicate.
   *
   * @param condition The condition to evaluate.
   * @param ifTrue The tasks to execute if the condition is true.
   * @param ifFalse The tasks to execute if the condition is false.
   *
   * @return A new WorkflowExecutor instance for the conditional branch.
   */
  public static <T> WorkflowExecutor<T> branch(Predicate<T> condition, List<AsyncTask<T>> ifTrue,
      List<AsyncTask<T>> ifFalse) {
    return branch(condition, ifTrue, ifFalse, null);
  }

  private void executeNext(T context, Consumer<T> onComplete) {
    if (currentIndex < tasks.size()) {
      AsyncTask<T> task = tasks.get(currentIndex);
      task.execute(context, continueExecution -> {
        if (Boolean.TRUE.equals(continueExecution)) {
          currentIndex++;
          executeNext(context, onComplete);
        } else {
          // Mark as failed and stop further execution
          failed = true;
          // Finalize the workflow without invoking the final completion callback
          if (completionCallback != null) {
            completionCallback.accept(context);
          }
        }
      });
    } else if (!failed) {
      // All tasks completed successfully
      if (completionCallback != null) {
        completionCallback.accept(context);
      }
      if (onComplete != null) {
        onComplete.accept(context);
      }
    }
  }
}
