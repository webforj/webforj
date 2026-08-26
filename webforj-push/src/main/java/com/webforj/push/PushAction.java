package com.webforj.push;

import java.io.Serializable;

/**
 * An action to display in a notification, opening its URL in the application when clicked.
 *
 * <p>
 * Safari does not implement notification actions, see the browser compatibility table of <a href=
 * "https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification#actions">showNotification</a>.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class PushAction implements Serializable {

  private final String action;
  private final String title;
  private final String url;

  /**
   * Creates an action.
   *
   * @param action a string that uniquely identifies this particular action within the actions of a
   *        message
   * @param title a string containing action text to be shown to the user
   * @param url the URL the click opens, resolved against the application root when relative
   *
   * @throws IllegalArgumentException when the action or the title is null or blank
   */
  public PushAction(String action, String title, String url) {
    if (action == null || action.isBlank()) {
      throw new IllegalArgumentException("The action identifier is required");
    }

    if (title == null || title.isBlank()) {
      throw new IllegalArgumentException("The action title is required");
    }

    this.action = action;
    this.title = title;
    this.url = url;
  }

  /**
   * Returns the string that uniquely identifies this particular action within the actions of a
   * message.
   *
   * @return the action identifier
   */
  public String getAction() {
    return action;
  }

  /**
   * Returns the action text to be shown to the user.
   *
   * @return the title
   */
  public String getTitle() {
    return title;
  }

  /**
   * Returns the URL the click opens.
   *
   * @return the URL, {@code null} when the click opens the URL of the message
   */
  public String getUrl() {
    return url;
  }
}
