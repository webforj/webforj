package org.dwcj;

/**
 * An application action which shows the given message when the application is terminated or error
 * is returned.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class MessageAction implements AppCloseAction {
  private final String message;

  /**
   * Creates a new instance of {@link MessageAction} with the given message.
   *
   * @param message the message to show
   */
  public MessageAction(String message) {
    this.message = message;
  }

  /**
   * Gets the message to show.
   *
   * @return the message to show
   */
  public String getMessage() {
    return message;
  }
}
