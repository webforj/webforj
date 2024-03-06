package com.webforj.component.navigator.event;

import com.webforj.component.navigator.Navigator;
import java.util.Map;

/**
 * A navigation event that indicates the user has navigated to the last page.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class NavigatorMoveLastEvent extends NavigatorMoveEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public NavigatorMoveLastEvent(Navigator component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
