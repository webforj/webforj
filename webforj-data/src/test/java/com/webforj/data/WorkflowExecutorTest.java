package com.webforj.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

class WorkflowExecutorTest {

  @Test
  void shouldExecuteSingleTask() {
    AtomicBoolean taskExecuted = new AtomicBoolean(false);
    List<WorkflowExecutor.AsyncTask<String>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> {
      taskExecuted.set(true);
      onComplete.accept(true);
    });

    WorkflowExecutor<String> executor = new WorkflowExecutor<>(tasks);
    executor.run("testContext");

    assertTrue(taskExecuted.get());
  }

  @Test
  void shouldExecuteMultipleTasksInOrder() {
    AtomicInteger taskOrder = new AtomicInteger(0);
    List<WorkflowExecutor.AsyncTask<Void>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> {
      assertEquals(0, taskOrder.getAndIncrement());
      onComplete.accept(true);
    });
    tasks.add((context, onComplete) -> {
      assertEquals(1, taskOrder.getAndIncrement());
      onComplete.accept(true);
    });

    WorkflowExecutor<Void> executor = new WorkflowExecutor<>(tasks);
    executor.run(null);

    assertEquals(2, taskOrder.get());
  }

  @Test
  void shouldNotContinueOnFalse() {
    AtomicBoolean task2Executed = new AtomicBoolean(false);
    List<WorkflowExecutor.AsyncTask<Void>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> onComplete.accept(false));
    tasks.add((context, onComplete) -> task2Executed.set(true));

    WorkflowExecutor<Void> executor = new WorkflowExecutor<>(tasks);
    executor.run(null);

    assertFalse(task2Executed.get());
  }

  @Test
  void shouldExecuteBranchBasedOnCondition() {
    AtomicBoolean thenBranchExecuted = new AtomicBoolean(false);
    AtomicBoolean elseBranchExecuted = new AtomicBoolean(false);

    List<WorkflowExecutor.AsyncTask<Void>> thenTasks = new ArrayList<>();
    thenTasks.add((context, onComplete) -> {
      thenBranchExecuted.set(true);
      onComplete.accept(true);
    });

    List<WorkflowExecutor.AsyncTask<Void>> elseTasks = new ArrayList<>();
    elseTasks.add((context, onComplete) -> {
      elseBranchExecuted.set(true);
      onComplete.accept(true);
    });

    WorkflowExecutor<Void> executor =
        WorkflowExecutor.branch(context -> false, thenTasks, elseTasks);
    executor.run(null);

    assertFalse(thenBranchExecuted.get());
    assertTrue(elseBranchExecuted.get());
  }

  @Test
  void shouldHandleNestedExecutors() {
    AtomicBoolean nestedTaskExecuted = new AtomicBoolean(false);

    List<WorkflowExecutor.AsyncTask<Void>> nestedTasks = new ArrayList<>();
    nestedTasks.add((context, onComplete) -> {
      nestedTaskExecuted.set(true);
      onComplete.accept(true);
    });

    List<WorkflowExecutor.AsyncTask<Void>> mainTasks = new ArrayList<>();
    mainTasks.add((context, onComplete) -> onComplete.accept(true));
    mainTasks.add((context, onComplete) -> new WorkflowExecutor<>(nestedTasks).run(context));

    WorkflowExecutor<Void> executor = new WorkflowExecutor<>(mainTasks);
    executor.run(null);

    assertTrue(nestedTaskExecuted.get());
  }

  @Test
  void shouldInvokeCompletionCallbackWhenDone() {
    AtomicBoolean completionCalled = new AtomicBoolean(false);
    List<WorkflowExecutor.AsyncTask<Void>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> onComplete.accept(true));

    WorkflowExecutor<Void> executor =
        new WorkflowExecutor<>(tasks, context -> completionCalled.set(true));
    executor.run(null);

    assertTrue(completionCalled.get());
  }

  @Test
  void shouldHandleAsynchronousTask() throws InterruptedException {
    AtomicBoolean taskCompleted = new AtomicBoolean(false);
    CountDownLatch latch = new CountDownLatch(1);

    List<WorkflowExecutor.AsyncTask<Void>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> {
      new Thread(() -> {
        try {
          Thread.sleep(500); // NOSONAR
          taskCompleted.set(true);
          onComplete.accept(true);
        } catch (InterruptedException e) {
          onComplete.accept(false);
        } finally {
          latch.countDown();
        }
      }).start();
    });

    WorkflowExecutor<Void> executor = new WorkflowExecutor<>(tasks);
    executor.run(null);

    latch.await();
    assertTrue(taskCompleted.get());
  }

  @Test
  void shouldStopAndResumeExecution() throws InterruptedException {
    AtomicBoolean task1Executed = new AtomicBoolean(false);
    AtomicBoolean task2Executed = new AtomicBoolean(false);
    CountDownLatch latch = new CountDownLatch(1);

    List<WorkflowExecutor.AsyncTask<Void>> tasks = new ArrayList<>();
    tasks.add((context, onComplete) -> {
      task1Executed.set(true);
      // Stop execution and wait for external signal to continue
      new Thread(() -> {
        try {
          Thread.sleep(1000); // NOSONAR
          onComplete.accept(true); // Resume execution
        } catch (InterruptedException e) {
          onComplete.accept(false);
        } finally {
          latch.countDown();
        }
      }).start();
    });
    tasks.add((context, onComplete) -> {
      task2Executed.set(true);
      onComplete.accept(true);
    });

    WorkflowExecutor<Void> executor = new WorkflowExecutor<>(tasks);
    executor.run(null);

    // Wait for the latch to ensure the test waits for the asynchronous task to complete
    latch.await();
    assertTrue(task1Executed.get());
    assertTrue(task2Executed.get());
  }

}
