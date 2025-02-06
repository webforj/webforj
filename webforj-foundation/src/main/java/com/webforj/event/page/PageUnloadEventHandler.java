package com.webforj.event.page;

import com.basis.bbj.proxies.event.BBjEvent;
import com.webforj.Page;
import com.webforj.dispatcher.EventDispatcher;

/**
 * Handler for the Page {@code BBjUnloadEvent} event.
 *
 * @author Hyyan Abo Fakher
 * @since 24.10
 */
public final class PageUnloadEventHandler {
  EventDispatcher dispatcher;
  Page page;

  /**
   * Construct a new PageUnloadEventHandler.
   *
   * @param page the page that is being unloaded
   * @param dispatcher the event dispatcher
   */
  public PageUnloadEventHandler(Page page, EventDispatcher dispatcher) {
    this.page = page;
    this.dispatcher = dispatcher;
  }

  /**
   * Handle the event.
   *
   * @param ev the event
   */
  public void handleEvent(BBjEvent ev) {
    dispatcher.dispatchEvent(new PageUnloadEvent(page));
    // cleanup everything related to the page
    dispatcher.removeAllListeners();
  }
}
