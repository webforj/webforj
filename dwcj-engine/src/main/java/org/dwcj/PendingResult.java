package org.dwcj;

import java.util.concurrent.CompletableFuture;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Represents an asynchronous operation that can be composed with other operations. This class is a
 * wrapper around {@link CompletableFuture}, providing a subset of its functionality while enabling
 * more straightforward composition and handling of asynchronous tasks. This class also provides
 * behavior similar to JavaScript promises.
 *
 * @param <T> The type of the result produced by the asynchronous operation.
 *
 * @author Hyyan Abo Fakher
 * @since 23.05
 */
public final class PendingResult<T> {
  final CompletableFuture<T> future;

  /**
   * Constructs a new PendingResult, initializing it with the {@link CompletableFuture} of an
   * existing PendingResult. This constructor is typically used for chaining or propagating the
   * state of an existing asynchronous operation.
   *
   * @param pending The PendingResult from which the CompletableFuture is copied.
   */
  public PendingResult(PendingResult<T> pending) {
    this.future = pending.future;
  }

  /**
   * Constructs a PendingResult with a specific {@link CompletableFuture}. This constructor is
   * useful for advanced use cases where specific behavior of CompletableFuture is required, such as
   * custom completion of the operation or integration with existing CompletableFuture instances.
   *
   * @param future The CompletableFuture to be wrapped in this PendingResult.
   */
  public PendingResult(CompletableFuture<T> future) {
    this.future = future;
  }

  /**
   * Constructs a PendingResult with a new, incomplete {@link CompletableFuture}. This default
   * constructor is used to initiate a fresh asynchronous operation.
   */
  public PendingResult() {
    this(new CompletableFuture<>());
  }

  /**
   * Transforms the result of this PendingResult with a provided function once the operation
   * completes. This method is similar to {@link CompletableFuture#thenApply(Function)}, allowing
   * for the transformation of the result without blocking for its completion.
   *
   * @param <U> The type of the result produced by the function.
   * @param fn The function to apply to the result of this PendingResult.
   *
   * @return A new PendingResult representing the result of applying the function.
   */
  public <U> PendingResult<U> thenApply(Function<? super T, ? extends U> fn) {
    return new PendingResult<>(future.thenApply(fn));
  }

  /**
   * Consumes the result of this PendingResult with a provided action once the operation completes.
   * This method is a non-blocking equivalent to {@link CompletableFuture#thenAccept(Consumer)},
   * allowing actions such as logging or notifications to be performed on the result.
   *
   * @param action The consumer to apply to the result.
   * @return A new PendingResult representing the completion of the action.
   */
  public PendingResult<Void> thenAccept(Consumer<? super T> action) {
    return new PendingResult<>(future.thenAccept(action));
  }

  /**
   * Handles exceptions that occur during the execution of the asynchronous operation. The provided
   * function is applied if an exception is thrown, allowing for custom error handling and recovery.
   *
   * @param fn The function to apply to the thrown exception.
   * @return A new PendingResult that will either contain the original result or the result of
   *         applying the function in case of an exception.
   */
  public PendingResult<T> exceptionally(Function<Throwable, ? extends T> fn) {
    return new PendingResult<>(future.exceptionally(fn));
  }

  /**
   * Composes this PendingResult with another asynchronous operation provided by the function
   * applied to the result of this operation. This method is akin to
   * {@link CompletableFuture#thenCompose(Function)}, enabling the sequential execution of
   * asynchronous operations.
   *
   * @param <U> The type of the result of the provided function.
   * @param fn The function to apply, which returns another PendingResult.
   *
   * @return A new PendingResult representing the result of the composed operations.
   */
  public <U> PendingResult<U> thenCompose(Function<? super T, PendingResult<U>> fn) {
    return new PendingResult<>(future.thenCompose(result -> fn.apply(result).future));
  }

  /**
   * Combines the result of this PendingResult with another PendingResult. The provided BiFunction
   * is applied to the results of both operations, creating a new result. This method is similar to
   * {@link CompletableFuture#thenCombine(CompletableFuture, BiFunction)}, facilitating the
   * combination of asynchronous operations.
   *
   * @param <U> The type of the result of the other PendingResult.
   * @param <V> The type of the result produced by the BiFunction.
   * @param other The other PendingResult to be combined with this one.
   * @param fn The BiFunction to apply to the results of both operations.
   *
   * @return A new PendingResult representing the result of the combined operations.
   */
  public <U, V> PendingResult<V> thenCombine(PendingResult<? extends U> other,
      BiFunction<? super T, ? super U, ? extends V> fn) {
    return new PendingResult<>(future.thenCombine(other.future, fn));
  }

  /**
   * Completes this PendingResult immediately with the provided result. This method is useful for
   * immediately resolving an asynchronous operation, similar to
   * {@link CompletableFuture#complete(Object)}.
   *
   * @param result The result with which to complete this PendingResult.
   * @return {@code true} if this PendingResult was completed by this invocation, {@code false}
   *         otherwise.
   */
  public boolean complete(T result) {
    return future.complete(result);
  }

  /**
   * Completes this PendingResult exceptionally with the provided exception. This method is used to
   * immediately complete the operation with an error, similar to
   * {@link CompletableFuture#completeExceptionally(Throwable)}.
   *
   * @param ex The exception with which to complete this PendingResult.
   */
  public void completeExceptionally(Throwable ex) {
    future.completeExceptionally(ex);
  }

  /**
   * Checks whether this PendingResult has completed. This method is a non-blocking way to determine
   * if the asynchronous operation has finished, akin to {@link CompletableFuture#isDone()}.
   *
   * @return {@code true} if this operation is complete, {@code false} otherwise.
   */
  public boolean isDone() {
    return future.isDone();
  }

  /**
   * Checks whether this PendingResult has completed exceptionally, i.e., with an error. This method
   * provides a non-blocking way to determine if the operation resulted in an exception, similar to
   * {@link CompletableFuture#isCompletedExceptionally()}.
   *
   * @return {@code true} if this operation completed exceptionally, {@code false} otherwise.
   */
  public boolean isCompletedExceptionally() {
    return future.isCompletedExceptionally();
  }

  /**
   * Creates and returns a new PendingResult that is already completed with the provided value. This
   * method is useful for returning a pre-completed asynchronous operation, similar to
   * {@link CompletableFuture#completedFuture(Object)}.
   *
   * @param <T> The type of the result.
   * @param value The value to complete the new PendingResult with.
   *
   * @return A new, already completed PendingResult with the given value.
   */
  public static <T> PendingResult<T> completedWith(T value) {
    CompletableFuture<T> completedFuture = CompletableFuture.completedFuture(value);
    return new PendingResult<>(completedFuture);
  }

  /**
   * Creates and returns a new PendingResult that is completed exceptionally with the provided
   * exception. This method is useful for creating an asynchronous operation that immediately
   * results in an error, allowing for testing or error propagation.
   *
   * @param <T> The type of the result.
   * @param ex The exception with which to complete the new PendingResult.
   *
   * @return A new PendingResult that is completed exceptionally with the given exception.
   */
  public static <T> PendingResult<T> completedExceptionallyWith(Throwable ex) {
    CompletableFuture<T> exceptionallyCompletedFuture = new CompletableFuture<>();
    exceptionallyCompletedFuture.completeExceptionally(ex);
    return new PendingResult<>(exceptionallyCompletedFuture);
  }
}
