package com.webforj.matchmedia.event;

import java.util.EventObject;
import java.util.Objects;

import com.webforj.matchmedia.MediaQueryList;

/**
 * A snapshot of a media query's match state reported by the browser.
 *
 * @since 26.03
 */
public final class MediaQueryChangeEvent extends EventObject {

  private final String media;
  private final boolean matched;
  private final boolean initial;

  /**
   * Creates a media query notification.
   *
   * @param source the query that produced the notification
   * @param media the browser's serialized media query
   * @param matched whether the query matched when the notification was produced
   * @param initial whether this is a requested initial notification rather than a change
   */
  public MediaQueryChangeEvent(MediaQueryList source, String media, boolean matched,
      boolean initial) {
    super(source);
    this.media = Objects.requireNonNull(media, "media");
    this.matched = matched;
    this.initial = initial;
  }

  @Override
  public MediaQueryList getSource() {
    return (MediaQueryList) super.getSource();
  }

  /**
   * Returns the browser's serialized media query.
   *
   * @return the media query
   */
  public String getMedia() {
    return media;
  }

  /**
   * Returns the match state captured with this notification.
   *
   * @return whether the query matched
   */
  public boolean isMatched() {
    return matched;
  }

  /**
   * Identifies the optional first notification requested when adding a listener.
   *
   * @return whether this notification reports the initial state
   */
  public boolean isInitial() {
    return initial;
  }
}
