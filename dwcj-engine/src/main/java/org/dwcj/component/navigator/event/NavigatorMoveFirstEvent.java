package org.dwcj.component.navigator.event;

import java.util.Map;
import org.dwcj.component.navigator.Navigator;

/**
 * A navigation event that indicates the user has navigated to the first page.
 *
 * @author Hyyan Abo Fakher
 * @since 24.00
 */
public class NavigatorMoveFirstEvent extends NavigatorMoveEvent {

  /**
   * Creates a new event.
   *
   * @param component the component
   * @param eventMap the event map
   */
  public NavigatorMoveFirstEvent(Navigator component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
