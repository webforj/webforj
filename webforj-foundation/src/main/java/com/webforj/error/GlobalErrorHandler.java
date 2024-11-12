package com.webforj.error;

import static com.webforj.App.console;

import java.util.logging.Logger;

/**
 * The {@code GlobalErrorHandler} class is used to handle errors that occur during the execution of
 * the webforJ application.
 *
 * <p>
 * {@code GlobalErrorHandler} will log the error to the console and display the error page with the
 * stack trace. Depending on the debug mode, the error page will display the stack trace or a
 * generic error message.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class GlobalErrorHandler implements ErrorHandler {
  private static final Logger log = Logger.getLogger("com.webforj.error.GlobalErrorHandler");

  /**
   * {@inheritDoc}
   */
  @Override
  public void onError(Throwable throwable, boolean debug) {
    // Log to BBj Debug log
    log.severe(throwable.getMessage());

    // log to the browser console (debug only)
    console().error(throwable);

    // display the error page with the stack trace
    String title = debug ? throwable.getClass().getName() : "500. That's an error";
    showErrorPage(title, StackTracePageBuilder.of(title, throwable, debug));
  }
}
