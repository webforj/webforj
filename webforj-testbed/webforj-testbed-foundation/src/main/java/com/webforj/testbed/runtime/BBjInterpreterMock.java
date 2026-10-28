package com.webforj.testbed.runtime;

import com.basis.startup.type.BBjException;
import java.lang.reflect.Method;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;

/**
 * The runtime mock backing the interpreter of the test context.
 *
 * <p>
 * Waiting returns immediately so that tests never sleep, and work submitted to the interpreter
 * context runs synchronously on the calling thread so that results are available deterministically.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public class BBjInterpreterMock extends RuntimeMock {

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      case "waitVerb", "processEvents", "processPendingEvents" -> null;
      case "isInterpreterContext" -> true;
      case "executeInInterpreterContext" -> {
        Callable<?> callable = (Callable<?>) args[0];
        try {
          yield CompletableFuture.completedFuture(callable.call());
        } catch (Exception e) {
          throw new BBjException(e, 0);
        }
      }
      default -> UNHANDLED;
    };
  }
}
