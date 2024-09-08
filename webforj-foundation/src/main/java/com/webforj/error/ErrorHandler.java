package com.webforj.error;

import com.webforj.NoneAction;
import com.webforj.Page;
import java.util.Base64;

/**
 * The {@code ErrorHandler} interface is designed to handle errors that occur during the execution
 * of a webforJ application.
 *
 * <p>
 * Applications can implement this interface to manage specific exceptions. The implementing class
 * should be named after the exception it handles, followed by the suffix {@code ErrorHandler}, and
 * should be placed in the META-INF/services directory. Additionally, it must be registered in the
 * {@code com.webforj.error.ErrorHandler} file. For example, to handle
 * {@code java.lang.NullPointerException}, the class should be named
 * {@code NullPointerExceptionErrorHandler} and placed in the META-INF/services directory, with an
 * entry in the {@code com.webforj.error.ErrorHandler} file.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public interface ErrorHandler {

  /**
   * Called when an error occurs.
   *
   * @param throwable the error that occurred.
   * @param debug {@code true} if debug mode is enabled, {@code false} otherwise.
   */
  void onError(Throwable throwable, boolean debug);

  /**
   * Displays the error page.
   *
   * <p>
   * This method will display the error page with the given title and content.
   * </p>
   *
   * <p>
   * If the application override the error or end actions then the page might not be displayed. The
   * error and end action should be set to {@link NoneAction} to display the page correctly.
   * </p>
   *
   * @param title The title of the error page.
   * @param content The content of the error page.
   */
  default void showErrorPage(String title, String content) {
    Base64.Encoder encoder = Base64.getEncoder();
    String encodedHtml = encoder.encodeToString(content.getBytes());

    String js = "document.body.innerHTML = atob('" + encodedHtml + "');";
    Page.getCurrent().executeJs(js);
    Page.getCurrent().setTitle(title);
  }
}
