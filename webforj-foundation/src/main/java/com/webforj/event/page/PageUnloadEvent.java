package com.webforj.event.page;

import com.webforj.Page;
import java.util.EventObject;

/**
 * An event that indicates that a page is unloaded by the browser.
 *
 * <p>
 * This event is fired when the browser unloads a page which can happen in different scenarios such
 * as when the user navigates to another page, closes the browser, refreshes the page, etc.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public class PageUnloadEvent extends EventObject {
  transient Page page;

  /**
   * Construct a new PageUnloadEvent.
   *
   * @param source the page that is being unloaded
   */
  public PageUnloadEvent(Page source) {
    super(source);
    this.page = source;
  }

  /**
   * Get the page that is being unloaded.
   *
   * @return the page that is being unloaded
   */
  public Page getPage() {
    return page;
  }
}
