package com.webforj.sink.page;

import com.webforj.Page;
import com.webforj.component.event.EventSinkListenerRegistry;
import com.webforj.event.page.PageEvent;

/**
 * {@code PageEventSinkRegistry} is used to manage the event listeners (add/remove) for a web
 * manager sink and the corresponding event.
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class PageEventSinkRegistry extends EventSinkListenerRegistry<PageEvent, Page> {

  /**
   * Creates a new PageEventSinkRegistry.
   *
   * @param sink The corresponding sink to the event
   * @param event The corresponding event to the sink
   */
  public PageEventSinkRegistry(PageEventSink sink, Class<? super PageEvent> event) {
    super(sink, event);
  }
}
