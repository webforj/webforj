package com.webforj.event.page;

import com.webforj.Page;
import com.webforj.PageVisibilityState;
import java.util.EventObject;

/**
 * An event that indicates the browser tab hosting the page has changed visibility state.
 *
 * <p>
 * The event is fired by the browser whenever the user switches tabs, minimizes the window, opens
 * the operating system task switcher, or otherwise causes the document to enter or leave the
 * background.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public class PageVisibilityChangeEvent extends EventObject {
  private final transient Page page;
  private final PageVisibilityState state;

  /**
   * Construct a new PageVisibilityChangeEvent.
   *
   * @param source the page that produced the event
   * @param state the visibility state reported by the browser
   */
  public PageVisibilityChangeEvent(Page source, PageVisibilityState state) {
    super(source);
    this.page = source;
    this.state = state;
  }

  /**
   * Returns the page that produced the event.
   *
   * @return the page
   */
  public Page getPage() {
    return page;
  }

  /**
   * Returns the visibility state reported by the browser.
   *
   * @return the visibility state
   */
  public PageVisibilityState getState() {
    return state;
  }

  /**
   * Returns {@code true} when the reported state is {@link PageVisibilityState#HIDDEN}.
   *
   * @return whether the page is hidden
   */
  public boolean isHidden() {
    return state == PageVisibilityState.HIDDEN;
  }

  /**
   * Returns {@code true} when the reported state is {@link PageVisibilityState#VISIBLE}.
   *
   * @return whether the page is visible
   */
  public boolean isVisible() {
    return state == PageVisibilityState.VISIBLE;
  }
}
