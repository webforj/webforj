package org.dwcj.component.tabbedpane.event;

import java.util.Map;
import org.dwcj.component.tabbedpane.TabbedPane;

/**
 * An event that is fired when a tab is selected.
 *
 * @author Hyyan Abo Fakher
 * @since 23.06
 */
public class TabSelectEvent extends TabEvent {

  /**
   * Constructs a new TabbedPaneSelectEvent.
   *
   * @param component the tabbed pane component
   * @param eventMap the event map
   */
  public TabSelectEvent(TabbedPane component, Map<String, Object> eventMap) {
    super(component, eventMap);
  }
}
