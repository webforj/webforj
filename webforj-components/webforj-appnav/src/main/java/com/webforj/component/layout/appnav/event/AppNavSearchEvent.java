package com.webforj.component.layout.appnav.event;

import com.webforj.component.element.annotation.EventName;
import com.webforj.component.element.annotation.EventOptions;
import com.webforj.component.element.annotation.EventOptions.EventData;
import com.webforj.component.event.ComponentEvent;
import com.webforj.component.layout.appnav.AppNav;
import java.util.Map;

/**
 * Emitted when a search is performed through the navigation's search field.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 *
 * @see AppNav
 */
@EventName(value = "dwc-searched")
@EventOptions(data = {@EventData(key = "term", exp = "event.detail")})
public final class AppNavSearchEvent extends ComponentEvent<AppNav> {

  /**
   * Creates a new {@code AppNavSearchEvent} with the given target and detail.
   *
   * @param target the target of the event
   * @param detail the detail of the event
   */
  public AppNavSearchEvent(AppNav target, Map<String, Object> detail) {
    super(target, detail);
  }

  /**
   * Gets the search term.
   *
   * @return the search term
   */
  public String getTerm() {
    return String.valueOf(getData().get("term"));
  }
}
