package com.webforj.devtools.livereload.receiver;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Listens for the virtual machine rejecting a class redefinition, without changing how the
 * rejection surfaces anywhere else.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
final class RedefinitionFailureHandler implements Thread.UncaughtExceptionHandler {

  static final String REDEFINITION_CLASS_NAME = "sun.instrument.InstrumentationImpl";
  static final String REDEFINITION_METHOD_NAME = "redefineClasses";

  private final Consumer<String> onRejection;
  private Thread.UncaughtExceptionHandler previous;
  private boolean installed;

  /**
   * Creates a handler that hands every rejection reason to the given consumer.
   *
   * @param onRejection the consumer the rejection reason is handed to
   */
  RedefinitionFailureHandler(Consumer<String> onRejection) {
    this.onRejection = onRejection;
  }

  /**
   * Enters the default uncaught exception chain, keeping the handler that is installed now as the
   * one every exception is forwarded to.
   */
  synchronized void install() {
    if (installed) {
      return;
    }

    previous = Thread.getDefaultUncaughtExceptionHandler();
    Thread.setDefaultUncaughtExceptionHandler(this);
    installed = true;
  }

  /**
   * Leaves the chain and restores the handler that was installed before, unless something else
   * replaced this handler in the meantime, then that replacement stays untouched.
   */
  synchronized void uninstall() {
    if (!installed) {
      return;
    }

    if (Thread.getDefaultUncaughtExceptionHandler() == this) {
      Thread.setDefaultUncaughtExceptionHandler(previous);
    }
    previous = null;
    installed = false;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void uncaughtException(Thread thread, Throwable exception) {
    try {
      String reason = rejectionReason(exception);
      if (reason != null) {
        onRejection.accept(reason);
      }
    } catch (RuntimeException e) {
      // The chain below must always run, so nothing raised here may replace the exception.
    }

    Thread.UncaughtExceptionHandler next = previous;
    if (next != null) {
      next.uncaughtException(thread, exception);
      return;
    }

    // The print the virtual machine performs when no handler is installed, reproduced verbatim,
    // so installing this handler changes nothing about how the exception surfaces.
    System.err.print("Exception in thread \"" + thread.getName() + "\" ");
    exception.printStackTrace(System.err);
  }

  private static String rejectionReason(Throwable exception) {
    Set<Throwable> seen = Collections.newSetFromMap(new IdentityHashMap<>());

    for (Throwable cause = exception; cause != null && seen.add(cause); cause = cause.getCause()) {
      if (cause instanceof UnsupportedOperationException && raisedByRedefinition(cause)) {
        String message = cause.getMessage();
        return message != null ? message : "class redefinition failed";
      }
    }

    return null;
  }

  private static boolean raisedByRedefinition(Throwable cause) {
    for (StackTraceElement frame : cause.getStackTrace()) {
      if (REDEFINITION_CLASS_NAME.equals(frame.getClassName())
          && frame.getMethodName().startsWith(REDEFINITION_METHOD_NAME)) {
        return true;
      }
    }

    return false;
  }
}
