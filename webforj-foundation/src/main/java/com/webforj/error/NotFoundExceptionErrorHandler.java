package com.webforj.error;

import static com.webforj.App.console;

/**
 * Error handler for 404 Not Found errors.
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public class NotFoundExceptionErrorHandler implements ErrorHandler {

  /**
   * {@inheritDoc}
   */
  @Override
  public void onError(Throwable throwable, boolean debug) {
    // Log to BBj Debug log
    System.err.println(throwable.getMessage()); // NOSONAR
    throwable.printStackTrace(); // NOSONAR

    // log to the browser console (debug only)
    console().error(throwable);

    String page =
        """
            <div style="margin: 0; display: flex; justify-content: center; align-items: center; height: 100vh; font-family: Arial, sans-serif;">
              <div style="text-align: center;">
                  <h1 style="font-size: 80px; margin: 0; color: var(--dwc-color-primary-text-dark);">404</h1>
                  <p style="font-size: 18px;">Sorry, the page you're looking for doesn't exist.</p>
              </div>
            </div>
              """;

    showErrorPage("404 Not Found", page);
  }
}
