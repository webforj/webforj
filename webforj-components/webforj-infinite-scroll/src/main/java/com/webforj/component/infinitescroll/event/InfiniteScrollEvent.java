package com.webforj.component.infinitescroll.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.infinitescroll.InfiniteScroll;
import java.util.Map;

/**
 * Emitted when the infinite scroll is triggered.
 *
 * @author Hyyan Abo Fakher
 * @since 25.00
 */
@EventName(value = "dwc-infinite")
@EventOptions(filter = "event.target.isSameNode(component)")
public final class InfiniteScrollEvent extends ComponentEvent<InfiniteScroll> {

  /**
   * Creates an infinite scroll Event.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public InfiniteScrollEvent(InfiniteScroll target, Map<String, Object> detail) {
    super(target, detail);
  }
}
