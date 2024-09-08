package com.webforj.error;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * The {@code ExceptionPage} class is responsible for building an exception page with the stack
 * trace.
 *
 * <p>
 * The exception page will display the stack trace of the exception if debug mode is enabled.
 * Otherwise, a generic error message will be displayed.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public final class StackTracePageBuilder {

  private StackTracePageBuilder() {
    // Prevent instantiation.
  }

  /**
   * Displays the exception page.
   *
   * @param title The title of the exception page.
   * @param throwable The exception to display.
   * @param debug {@code true} if debug mode is enabled, {@code false} otherwise.
   *
   * @return The HTML content of the exception page.
   */
  public static String of(String title, Throwable throwable, boolean debug) {
    String page = "";

    if (debug) {
      StringWriter trace = new StringWriter();
      PrintWriter pw = new PrintWriter(trace);
      throwable.printStackTrace(pw);

      String message = throwable.getLocalizedMessage();

      StringBuilder html = new StringBuilder();
      html.append("<h1><strong>").append(title).append("</strong></h1>");
      if (message != null && !message.isEmpty()) {
        html.append("<pre style='white-space: pre-wrap; overflow-wrap: break-word;'>")
            .append(message).append("</pre>");
      }
      html.append("<h3><strong>Caused by:</strong></h3>");
      html.append("<pre style='white-space: pre-wrap; overflow-wrap: break-word;'>")
          .append(trace.toString()).append("</pre>");
      page += html.toString();
    } else {
      StringBuilder pageBuilder = new StringBuilder();
      pageBuilder
          .append("<div style='display: flex; justify-content: center; align-items: center; ");
      pageBuilder.append("height: 100vh; flex-direction: column;'>");
      pageBuilder.append("<h1> <strong>500.</strong> Thats an error </h1>");
      pageBuilder
          .append("<p> The server encountered an error and could not complete your request. </p>");
      pageBuilder.append(
          "<p> If the problem persists, please contact the application administrator. </p>");
      pageBuilder.append("<a href='javascript:location.reload();'>Retry again</a>");
      pageBuilder.append("</div>");
      page = pageBuilder.toString();
    }

    return page;
  }
}
