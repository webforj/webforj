package org.dwcj;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * An application action which opens the given url when the application is terminated or error is
 * returned.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public final class RedirectAction implements AppCloseAction {
  private final URL url;

  /**
   * Creates a new instance of {@link RedirectAction} with the given url.
   *
   * @param url the url to open
   */
  public RedirectAction(URL url) {
    this.url = url;
  }

  /**
   * Creates a new instance of {@link RedirectAction} with the given url string.
   *
   * @param url the url string to open
   */
  public RedirectAction(String url) {
    URL tempUrl = null;
    try {
      tempUrl = new URL(url);
    } catch (MalformedURLException e) {
      throw new IllegalStateException("Invalid url: " + url);
    }

    this.url = tempUrl;
  }

  /**
   * Gets the url to open.
   *
   * @return the url to open
   */
  public URL getUrl() {
    return url;
  }
}
